module XmlParser exposing (..)

import Parser exposing (..)
import Char


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
        |. whiteSpace
        |. symbol "<"
        |= tagName
        |. whiteSpace
        |= repeat zeroOrMore attribute
        |. whiteSpace
        |= (oneOf
                [ succeed []
                    |. symbol "/>"
                , succeed identity
                    |. symbol ">"
                    |. whiteSpace
                    |= children
                ]
           )
        |. whiteSpace


tagName : Parser String
tagName =
    keep oneOrMore (\c -> Char.isLower c)


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
        , succeed List.singleton
            |= text
        ]


text : Parser Node
text =
    succeed Text
        |= keep oneOrMore ((/=) '<')


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
        |= keep oneOrMore ((/=) '"')
        |. symbol "\""


whiteSpace : Parser ()
whiteSpace =
    ignore zeroOrMore ((==) ' ')
