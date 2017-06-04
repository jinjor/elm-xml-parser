module XmlParser
    exposing
        ( Xml
        , ProcessingInstruction
        , DocType
        , DocTypeDefinition(..)
        , Node(..)
        , Attribute
        , parse
        )

import Parser exposing (..)
import Char
import Set exposing (Set)
import Dict exposing (Dict)
import Hex


type alias Xml =
    { processingInstructions : List ProcessingInstruction
    , docType : Maybe DocType
    , root : Node
    }


type alias ProcessingInstruction =
    { name : String
    , value : String
    }


type alias DocType =
    { rootElementName : String
    , definition : DocTypeDefinition
    }


type DocTypeDefinition
    = Public String String (Maybe String)
    | System String (Maybe String)
    | Custom String


type Node
    = Element String (List Attribute) (List Node)
    | Text String


type alias Attribute =
    { name : String, value : String }


parse : String -> Result Parser.Error Xml
parse source =
    Parser.run xml source


xml : Parser Xml
xml =
    inContext "xml" <|
        succeed Xml
            |. whiteSpace
            |= repeat zeroOrMore
                (succeed identity
                    |= processingInstruction
                    |. whiteSpace
                )
            |= maybe docType
            |. whiteSpace
            |= element
            |. whiteSpace
            |. end


processingInstruction : Parser ProcessingInstruction
processingInstruction =
    inContext "processingInstruction" <|
        succeed ProcessingInstruction
            |. symbol "<?"
            |= processingInstructionName
            |. symbol " "
            |= processingInstructionValue


processingInstructionName : Parser String
processingInstructionName =
    inContext "processingInstructionName" <|
        keep oneOrMore (\c -> c /= ' ')


processingInstructionValue : Parser String
processingInstructionValue =
    inContext "processingInstructionValue" <|
        oneOf
            [ succeed ""
                |. symbol "?>"
            , symbol "?"
                |> andThen
                    (\_ ->
                        processingInstructionValue
                            |> map (\tail -> "?" ++ tail)
                    )
            , succeed (++)
                |= keep zeroOrMore (\c -> c /= '?')
                |= lazy (\_ -> processingInstructionValue)
            ]


docType : Parser DocType
docType =
    inContext "docType" <|
        succeed DocType
            |. symbol "<!DOCTYPE"
            |. whiteSpace
            |= tagName
            |. whiteSpace
            |= docTypeDefinition
            |. whiteSpace
            |. symbol ">"


docTypeDefinition : Parser DocTypeDefinition
docTypeDefinition =
    inContext "docTypeDefinition" <|
        oneOf
            [ succeed Public
                |. keyword "PUBLIC"
                |. whiteSpace
                |= publicIdentifier
                |. whiteSpace
                |= docTypeExternalSubset
                |. whiteSpace
                |= maybe docTypeInternalSubset
            , succeed System
                |. keyword "SYSTEM"
                |. whiteSpace
                |= docTypeExternalSubset
                |. whiteSpace
                |= maybe docTypeInternalSubset
            , succeed Custom
                |= docTypeInternalSubset
            ]


publicIdentifier : Parser String
publicIdentifier =
    inContext "publicIdentifier" <|
        succeed identity
            |. symbol "\""
            |= keep zeroOrMore (\c -> c /= '"')
            |. symbol "\""


docTypeExternalSubset : Parser String
docTypeExternalSubset =
    inContext "docTypeExternalSubset" <|
        succeed identity
            |. symbol "\""
            |= keep zeroOrMore (\c -> c /= '"')
            |. symbol "\""


docTypeInternalSubset : Parser String
docTypeInternalSubset =
    inContext "docTypeInternalSubset" <|
        succeed identity
            |. symbol "["
            |= keep zeroOrMore (\c -> c /= ']')
            |. symbol "]"


cdata : Parser String
cdata =
    inContext "cdata" <|
        succeed identity
            |. symbol "<![CDATA["
            |= cdataContent


cdataContent : Parser String
cdataContent =
    inContext "cdataContent" <|
        oneOf
            [ succeed ""
                |. symbol "]]>"
            , symbol "]]"
                |> andThen
                    (\_ ->
                        cdataContent
                            |> map (\tail -> "]]" ++ tail)
                    )
            , symbol "]"
                |> andThen
                    (\_ ->
                        cdataContent
                            |> map (\tail -> "]" ++ tail)
                    )
            , succeed (++)
                |= keep zeroOrMore (\c -> c /= ']')
                |= lazy (\_ -> cdataContent)
            ]


element : Parser Node
element =
    inContext "element" <|
        succeed identity
            |. symbol "<"
            |= (tagName
                    |> andThen
                        (\startTagName ->
                            succeed (Element startTagName)
                                |. whiteSpace
                                |= attributes Set.empty
                                |. whiteSpace
                                |= oneOf
                                    [ succeed []
                                        |. symbol "/>"
                                    , succeed identity
                                        |. symbol ">"
                                        |= lazy (\_ -> children startTagName)
                                    ]
                        )
               )


tagName : Parser String
tagName =
    inContext "tagName" <|
        keep oneOrMore (\c -> not (isWhitespace c) && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


children : String -> Parser (List Node)
children startTagName =
    inContext "children" <|
        oneOf
            [ succeed []
                |. closingTag startTagName
            , textNodeString
                |> andThen
                    (\maybeString ->
                        case maybeString of
                            Just s ->
                                succeed (\rest -> Text s :: rest)
                                    |= children startTagName

                            Nothing ->
                                succeed []
                                    |. closingTag startTagName
                    )
            , lazy
                (\_ ->
                    succeed (::)
                        |= element
                        |= children startTagName
                )
            ]


closingTag : String -> Parser ()
closingTag startTagName =
    inContext "closingTag" <|
        succeed ()
            |. symbol "</"
            |. whiteSpace
            |. (tagName
                    |> andThen
                        (\endTagName ->
                            if startTagName == endTagName then
                                succeed ()
                            else
                                fail ("tag name mismatch: " ++ startTagName ++ " and " ++ endTagName)
                        )
               )
            |. whiteSpace
            |. symbol ">"


textString : Char -> Parser String
textString end =
    inContext "textString" <|
        (keep zeroOrMore (\c -> c /= end && c /= '&')
            |> andThen
                (\s ->
                    oneOf
                        [ succeed String.cons
                            |= escapedChar end
                            |= lazy (\_ -> textString end)
                        , succeed s
                        ]
                )
        )


textNodeString : Parser (Maybe String)
textNodeString =
    inContext "textNodeString" <|
        oneOf
            [ succeed
                (\s maybeString ->
                    Just (s ++ (maybeString |> Maybe.withDefault ""))
                )
                |= keep oneOrMore (\c -> c /= '<' && c /= '&')
                |= lazy (\_ -> textNodeString)
            , succeed
                (\c maybeString ->
                    Just (String.cons c (maybeString |> Maybe.withDefault ""))
                )
                |= escapedChar '<'
                |= lazy (\_ -> textNodeString)
            , succeed
                (\s maybeString ->
                    let
                        str =
                            s ++ (maybeString |> Maybe.withDefault "")
                    in
                        if str /= "" then
                            Just str
                        else
                            Nothing
                )
                |= cdata
                |= lazy (\_ -> textNodeString)
            , succeed Nothing
            ]


escapedChar : Char -> Parser Char
escapedChar end =
    inContext "escapedChar" <|
        (succeed identity
            |. symbol "&"
            |= keep oneOrMore (\c -> c /= end && c /= ';')
            |> andThen
                (\s ->
                    oneOf
                        [ symbol ";"
                            |> andThen
                                (\_ ->
                                    case decodeEscape s of
                                        Ok c ->
                                            succeed c

                                        Err e ->
                                            fail e
                                )
                        , fail ("Entities must end with \";\": &" ++ s)
                        ]
                )
        )


decodeEscape : String -> Result String Char
decodeEscape s =
    if String.startsWith "#x" s then
        s
            |> String.dropLeft 2
            |> Hex.fromString
            |> Result.map Char.fromCode
    else if String.startsWith "#" s then
        s
            |> String.dropLeft 1
            |> String.toInt
            |> Result.map Char.fromCode
    else
        Dict.get s entities
            |> Result.fromMaybe ("No entity named \"&" ++ s ++ ";\" found.")


entities : Dict String Char
entities =
    Dict.fromList
        [ "amp" => '&'
        , "lt" => '<'
        , "gt" => '>'
        , "apos" => '\''
        , "quot" => '"'
        ]


attributes : Set String -> Parser (List Attribute)
attributes keys =
    inContext "attributes" <|
        oneOf
            [ attribute
                |> andThen
                    (\attr ->
                        if Set.member attr.name keys then
                            fail ("attribute " ++ attr.name ++ " is duplicated")
                        else
                            succeed ((::) attr)
                                |. whiteSpace
                                |= attributes (Set.insert attr.name keys)
                    )
            , succeed []
            ]


validateAttributes : Set String -> List Attribute -> Maybe String
validateAttributes keys attrs =
    case attrs of
        [] ->
            Nothing

        x :: xs ->
            if Set.member x.name keys then
                Just x.name
            else
                validateAttributes (Set.insert x.name keys) xs


attribute : Parser Attribute
attribute =
    inContext "attribute" <|
        succeed Attribute
            |= attributeName
            |. whiteSpace
            |. symbol "="
            |. whiteSpace
            |= attributeValue


attributeName : Parser String
attributeName =
    inContext "attributeName" <|
        keep oneOrMore (\c -> not (isWhitespace c) && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


attributeValue : Parser String
attributeValue =
    inContext "attributeValue" <|
        oneOf
            [ succeed identity
                |. symbol "\""
                |= textString '"'
                |. symbol "\""
            , succeed identity
                |. symbol "'"
                |= textString '\''
                |. symbol "'"
            ]


whiteSpace : Parser ()
whiteSpace =
    ignore zeroOrMore isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\x0D' || c == '\n' || c == '\t'


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]



-- POLYFILL


(=>) =
    (,)
