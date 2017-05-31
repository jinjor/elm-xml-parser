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
    keep oneOrMore (\c -> c /= ' ' && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')


children : Parser (List Node)
children =
    oneOf
        [ succeed []
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
                    |= map (String.join "" >> Text) (repeat oneOrMore textString)
                    |= children
            )
        ]


textString : Parser String
textString =
    oneOf
        [ maybeEscapedString
        , nonEscapedString
        ]


nonEscapedString : Parser String
nonEscapedString =
    keep oneOrMore (\c -> c /= '<' && c /= '&')


maybeEscapedString : Parser String
maybeEscapedString =
    succeed identity
        |. symbol "&"
        |= keep oneOrMore (\c -> c /= '<' && c /= ';')
        |> andThen
            (\s ->
                oneOf
                    [ symbol ";"
                        |> andThen
                            (\_ ->
                                case decodeEscape s of
                                    Ok c ->
                                        succeed (String.fromChar c)

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
