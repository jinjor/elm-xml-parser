module Example exposing (..)

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
        [ test "tagName1" <| expectSucceed "<a/>" (Element "a" [] [])
        , test "tagName2" <| expectFail "</>"
        ]
