module XmlParser exposing (..)

import Parser exposing (..)
import Char
import Dict exposing (Dict)
import Hex


type Node
    = Element String (List Attribute) (List Node)
    | Text String


type alias Attribute =
    { name : String, value : String }


parse : String -> Result Parser.Error Node
parse source =
    Parser.run element source


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
    keep oneOrMore (\c -> c /= ' ' && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'')


children : Parser (List Node)
children =
    oneOf
        [ succeed []
            |. symbol "</"
            |. whiteSpace
            |. tagName
            |. whiteSpace
            |. symbol ">"
          -- , succeed (::)
          --     |= element
          --     |= lazy (\_ -> children)
        , succeed (::)
            |= map (String.join "" >> Text) (repeat oneOrMore textString)
            |= lazy (\_ -> children)
        ]


textString : Parser String
textString =
    oneOf
        [ map String.fromChar escapedChar
        , nonEscapedString
        ]


nonEscapedString : Parser String
nonEscapedString =
    keep oneOrMore (\c -> c /= '<' && c /= '&')


escapedChar : Parser Char
escapedChar =
    succeed identity
        |. symbol "&"
        |= escapedCharHelp
        |. symbol ";"


escapedCharHelp : Parser Char
escapedCharHelp =
    oneOf
        [ succeed Number
            |. symbol "#"
        , succeed Word
        ]
        |> andThen
            (\escapeType ->
                keep oneOrMore (\c -> c /= ';')
                    |> andThen
                        (\s ->
                            case decodeEscape escapeType s of
                                Ok c ->
                                    succeed c

                                Err s ->
                                    fail s
                        )
            )


type EscapeType
    = Number
    | Word


decodeEscape : EscapeType -> String -> Result String Char
decodeEscape escapeType s =
    case escapeType of
        Number ->
            Hex.fromString s
                |> Result.map Char.fromCode

        Word ->
            Dict.get s entities
                |> Result.fromMaybe ("No entity named \"&" ++ s ++ ";\"")


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
    keep oneOrMore (\c -> Char.isUpper c || Char.isLower c)


attributeValue : Parser String
attributeValue =
    succeed identity
        |. symbol "\""
        |= keep zeroOrMore ((/=) '"')
        |. symbol "\""


whiteSpace : Parser ()
whiteSpace =
    ignore zeroOrMore ((==) ' ')



-- POLYFILL


(=>) =
    (,)
