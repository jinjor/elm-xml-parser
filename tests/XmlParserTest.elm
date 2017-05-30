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
        [ test "tagName1" <| expectSucceed "<a1/>" (Element "a1" [] [])
        , test "tagName2" <| expectFail "</>"
        , test "tagName3" <| expectFail "<a>"
        , test "tagName4" <| expectFail "<1>"
        , test "attribute1" <| expectSucceed """<a b=""/>""" (Element "a" [ Attribute "b" "" ] [])
        , test "attribute2" <| expectSucceed """<a b="1=</>"/>""" (Element "a" [ Attribute "b" "1=</>" ] [])
        , test "attribute3" <| expectFail """<a 1=""/>"""
        , test "attribute4" <| expectFail """<a a=/>"""
        , test "attribute5" <| expectFail """<a a"="/>"""
        , test "attribute6" <| expectFail """<a=""/>"""
        , test "closing1" <| expectSucceed "<a></a>" (Element "a" [] [])
        , test "closing2" <| expectFail "<a></b>"
        , test "children1" <| expectSucceed "<a>1</a>" (Element "a" [] [ Text "1" ])
        ]
