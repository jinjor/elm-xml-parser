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
    succeed Xml
        |= repeat zeroOrMore processingInstruction
        |. whiteSpace
        |= maybe docType
        |. whiteSpace
        |= element
        |. end


processingInstruction : Parser ProcessingInstruction
processingInstruction =
    succeed ProcessingInstruction
        |. symbol "<?"
        |= processingInstructionName
        |. symbol " "
        |= processingInstructionValue
        |. symbol "?>"


processingInstructionName : Parser String
processingInstructionName =
    keep oneOrMore (\c -> c /= ' ')


processingInstructionValue : Parser String
processingInstructionValue =
    oneOf
        [ succeed ""
            |. symbol "?>"
        , symbol "?"
            |> andThen
                (\_ ->
                    processingInstructionName
                        |> map (\tail -> "?" ++ tail)
                )
        , keep zeroOrMore (\c -> c /= '?')
        ]


type alias DocType =
    { rootElementName : String
    , definition : DocTypeDefinition
    }


type DocTypeDefinition
    = Public String String (Maybe String)
    | System String (Maybe String)
    | Custom String


docType : Parser DocType
docType =
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
    succeed identity
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '"')
        |. symbol "\""


docTypeExternalSubset : Parser String
docTypeExternalSubset =
    succeed identity
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '"')
        |. symbol "\""


docTypeInternalSubset : Parser String
docTypeInternalSubset =
    succeed identity
        |. symbol "["
        |= keep zeroOrMore (\c -> c /= ']')
        |. symbol "]"


cdata : Parser String
cdata =
    succeed identity
        |. symbol "<![CDATA["
        |= cdataContent
        |. symbol "]]>"


cdataContent : Parser String
cdataContent =
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
        , keep zeroOrMore (\c -> c /= ']')
        ]


element : Parser Node
element =
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
    keep oneOrMore (\c -> c /= ' ' && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


children : Parser (List Node)
children =
    oneOf
        [ andThen (\_ -> fail "") end
        , succeed []
            |. symbol "</"
            |. whiteSpace
            |. tagName
            |. whiteSpace
            |. symbol ">"
        , lazy
            (\_ ->
                succeed (::)
                    |= element
                    |= children
            )
        , lazy
            (\_ ->
                succeed (::)
                    |= map Text (textString '<')
                    |= children
            )
        ]


textString : Char -> Parser String
textString end =
    keep zeroOrMore (\c -> c /= end && c /= '&')
        |> andThen
            (\s ->
                oneOf
                    [ succeed String.cons
                        |= escapedChar end
                        |= lazy (\_ -> textString end)
                    , succeed s
                    ]
            )


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
