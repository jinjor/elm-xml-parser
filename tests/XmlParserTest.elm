module XmlParserTest exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (list, int, string)
import XmlParser exposing (..)


expectSucceed : String -> Node -> (() -> Expectation)
expectSucceed source node =
    \_ ->
        case XmlParser.parse source of
            Ok ast ->
                Expect.equal ast.root node

            Err e ->
                Expect.fail (toString e)


expectFail : String -> (() -> Expectation)
expectFail source =
    \_ ->
        case XmlParser.parse source of
            Ok ast ->
                Expect.fail ("Unexpectedly succeeded: " ++ toString ast)

            Err e ->
                Expect.pass


suite : Test
suite =
    describe "XmlParser"
        [ test "tagName" <| expectSucceed "<a/>" (Element "a" [] [])
        , test "tagName number" <| expectSucceed "<1/>" (Element "1" [] [])
        , test "tagName unicode 1" <| expectSucceed "<あ/>" (Element "あ" [] [])
        , test "tagName unicode 2" <| expectSucceed "<😄/>" (Element "😄" [] [])
        , test "tagName surrogate pairs" <| expectSucceed "<𩸽/>" (Element "𩸽" [] [])
        , test "tagName namespace" <| expectSucceed "<a:b/>" (Element "a:b" [] [])
        , test "tagName fail 1" <| expectFail "</>"
          -- , test "tagName fail 2" <| expectFail "<a>"
          -- , test "tagName fail 3" <| expectFail "<1>"
        , test "attribute 1" <| expectSucceed """<a b=""/>""" (Element "a" [ Attribute "b" "" ] [])
        , test "attribute 2" <| expectSucceed """<a b="1=</>"/>""" (Element "a" [ Attribute "b" "1=</>" ] [])
        , test "attribute quote 1" <| expectSucceed """<a b='""'/>""" (Element "a" [ Attribute "b" "\"\"" ] [])
        , test "attribute quote 2" <| expectSucceed """<a b="''"/>""" (Element "a" [ Attribute "b" "''" ] [])
        , test "attribute key number" <| expectSucceed """<a 1=""/>""" (Element "a" [ Attribute "1" "" ] [])
        , test "attribute key unicode 1" <| expectSucceed """<a あ=""/>""" (Element "a" [ Attribute "あ" "" ] [])
        , test "attribute key unicode 2" <| expectSucceed """<a 😄=""/>""" (Element "a" [ Attribute "😄" "" ] [])
        , test "attribute key surrogate pairs" <| expectSucceed """<a 𩸽=""/>""" (Element "a" [ Attribute "𩸽" "" ] [])
        , test "attribute key namespace" <| expectSucceed """<a b:c=""/>""" (Element "a" [ Attribute "b:c" "" ] [])
        , test "attribute fail 1" <| expectFail """<a a=/>"""
        , test "attribute fail 2" <| expectFail """<a a"="/>"""
        , test "attribute fail 3" <| expectFail """<a=""/>"""
        , test "attribute fail 4" <| expectFail """<a b c=""/>"""
        , test "attribute fail 5" <| expectFail """<a= b=""/>"""
        , test "attribute value escape 1" <| expectSucceed """<a a="&quot;"/>""" (Element "a" [ Attribute "a" "\"" ] [])
          -- , test "attribute fail same names" <| expectFail """<a b="" b=""/>"""
        , test "closing 1" <| expectSucceed "<a></a>" (Element "a" [] [])
          -- , test "closing 2" <| expectFail "<a></b>"
        , test "children text" <| expectSucceed "<a>1</a>" (Element "a" [] [ Text "1" ])
        , test "children text escape 1" <| expectSucceed "<a>&amp;</a>" (Element "a" [] [ Text "&" ])
        , test "children text escape 2" <| expectSucceed "<a>&#x41;</a>" (Element "a" [] [ Text "A" ])
        , test "children text escape fail" <| expectFail "<a>&&;</a>"
        , test "children element 1" <| expectSucceed "<a><b/></a>" (Element "a" [] [ Element "b" [] [] ])
        , test "children element 2" <| expectSucceed "<a><b/><c></c></a>" (Element "a" [] [ Element "b" [] [], Element "c" [] [] ])
        , test "children element 3" <|
            expectSucceed "<a>1<b/>2<c>3</c>4</a>"
                (Element "a" [] [ Text "1", Element "b" [] [], Text "2", Element "c" [] [ Text "3" ], Text "4" ])
          -- , test "children element fail" <| expectFail "<a><b></a></a>"
        ]



-- memo
-- CDATA
-- <?
-- DOCTYPE
