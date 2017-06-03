module XmlParser exposing (..)

import Parser exposing (..)
import Char
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
            |= repeat zeroOrMore processingInstruction
            |. whiteSpace
            |= maybe docType
            |. whiteSpace
            |= element
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
        succeed Element
            |. symbol "<"
            |= tagName
            |. whiteSpace
            |= repeat zeroOrMore attribute
            |. whiteSpace
            |= oneOf
                [ succeed []
                    |. symbol "/>"
                , succeed identity
                    |. symbol ">"
                    |. whiteSpace
                    |= lazy (\_ -> children)
                ]


tagName : Parser String
tagName =
    inContext "tagName" <|
        keep oneOrMore (\c -> c /= ' ' && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


children : Parser (List Node)
children =
    inContext "children" <|
        oneOf
            [ succeed []
                |. closingTag
            , lazy
                (\_ ->
                    succeed (::)
                        |= element
                        |= children
                )
            , textNodeString
                |> andThen
                    (\maybeString ->
                        case maybeString of
                            Just s ->
                                succeed (\rest -> Text s :: rest)
                                    |= children

                            Nothing ->
                                succeed []
                                    |. closingTag
                    )
            ]


closingTag : Parser ()
closingTag =
    inContext "closingTag" <|
        succeed ()
            |. symbol "</"
            |. whiteSpace
            |. tagName
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
    inContext "textString" <|
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
                    Just (s ++ (maybeString |> Maybe.withDefault ""))
                )
                |= cdata
                |= lazy (\_ -> textNodeString)
            , succeed Nothing
            ]


maybeTextString : Char -> Parser (Maybe String)
maybeTextString end =
    inContext "maybeTextString" <|
        succeed
            (\s ->
                if String.trim s == "" then
                    Nothing
                else
                    Just s
            )
            |= textString end


escapedChar : Char -> Parser Char
escapedChar end =
    succeed identity
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


decodeEscape : String -> Result String Char
decodeEscape s =
    if String.startsWith "#x" s then
        s
            |> String.dropLeft 2
            |> Hex.fromString
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


attribute : Parser Attribute
attribute =
    succeed Attribute
        |= attributeName
        |. whiteSpace
        |. symbol "="
        |. whiteSpace
        |= attributeValue
        |. whiteSpace


attributeName : Parser String
attributeName =
    keep oneOrMore (\c -> c /= ' ' && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


attributeValue : Parser String
attributeValue =
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
    ignore zeroOrMore ((==) ' ')


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]



-- POLYFILL


(=>) =
    (,)
