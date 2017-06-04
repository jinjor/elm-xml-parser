port module Demo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import XmlParser exposing (..)


port init : (String -> msg) -> Sub msg


port input : (String -> msg) -> Sub msg


port parse : ({} -> msg) -> Sub msg


main : Program Never Model Msg
main =
    program
        { init = initialModel ! []
        , update = \msg model -> (update msg model) ! []
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ init Init
                    , input Input
                    , parse (always Show)
                    ]
        }


type alias Model =
    { src : String
    , dest : String
    }


initialModel : Model
initialModel =
    { src = ""
    , dest = ""
    }


type Msg
    = Init String
    | Input String
    | Show


update : Msg -> Model -> Model
update msg model =
    case msg of
        Init s ->
            { model | src = s } |> show

        Input s ->
            { model | src = s }

        Show ->
            model |> show


show : Model -> Model
show model =
    { model
        | dest =
            toString (XmlParser.parse model.src)
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style [] ] [ text "XmlParser DEMO" ]
        , p []
            [ text "Press Ctrl+S to show result ("
            , a [ href "https://github.com/jinjor/elm-xml-parser" ] [ text "Source" ]
            , text ")"
            ]
        , div [ style [ ( "display", "flex" ) ] ]
            [ div [ id "editor", style [ ( "height", "500px" ), ( "width", "40%" ), ( "border", "solid 1px #aaa" ) ] ] [ text initialText ]
            , div [ style [ ( "min-height", "500px" ), ( "width", "60%" ), ( "flex-grow", "1" ), ( "border", "solid 1px #aaa" ) ] ] [ text model.dest ]
            ]
        ]


initialText : String
initialText =
    """<note>
<to>Tove</to>
<from>Jani</from>
<heading>Reminder</heading>
<body>Don't forget me this weekend!</body>
</note>
"""
