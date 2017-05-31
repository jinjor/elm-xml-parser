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
                Expect.equal ast node

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
        , test "tagName 2" <| expectFail "</>"
        , test "tagName 3" <| expectFail "<a>"
        , test "tagName 4" <| expectFail "<1>"
        , test "attribute 1" <| expectSucceed """<a b=""/>""" (Element "a" [ Attribute "b" "" ] [])
        , test "attribute 2" <| expectSucceed """<a b="1=</>"/>""" (Element "a" [ Attribute "b" "1=</>" ] [])
        , test "attribute 3" <| expectFail """<a 1=""/>"""
        , test "attribute 4" <| expectFail """<a a=/>"""
        , test "attribute 5" <| expectFail """<a a"="/>"""
        , test "attribute 6" <| expectFail """<a=""/>"""
        , test "closing 1" <| expectSucceed "<a></a>" (Element "a" [] [])
          -- , test "closing 2" <| expectFail "<a></b>"
        , test "children 1" <| expectSucceed "<a>1</a>" (Element "a" [] [ Text "1" ])
        , test "children 2" <| expectSucceed "<a>&amp;</a>" (Element "a" [] [ Text "&" ])
        , test "children 3" <| expectSucceed "<a>&amp</a>" (Element "a" [] [ Text "&amp" ])
        ]



-- memo
-- CDATA
-- <?
-- DOCTYPE
-- &amp, etc.
