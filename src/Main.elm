module Main exposing (..)

import Browser exposing (element)
import Debug exposing (log)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--- TYPES


type Msg
    = Noop


type Model
    = Loading


testPayload : String
testPayload =
    """
{"tile": "tile.png",
 "pois": [{"name": "Market1", "coords": [22, 56]}
         ]}
"""


type alias MapPayload =
    { tile : String
    , pois : List POI
    }


type alias POI =
    { name : String
    , coords : ( Int, Int )
    }


pair : a -> a -> ( a, a )
pair x y =
    ( x, y )


mapDecoder : D.Decoder MapPayload
mapDecoder =
    let
        dCoords =
            D.map2 pair D.int D.int

        dPOI =
            D.succeed POI
                |> required "name" D.string
                |> required "coords" dCoords
    in
    D.succeed MapPayload
        |> required "tile" D.string
        |> required "pois" (D.list dPOI)



--- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )



--- VIEW


view : Model -> Html Msg
view model =
    layout [] (uiView model)


uiView : Model -> Element Msg
uiView model =
    column [ width fill ]
        [ viewMap model
        ]


viewMap : Model -> Element Msg
viewMap model =
    let
        topRow =
            row [ width fill ] [ txt "^" ]

        midRow =
            row [ width fill ] [ txt "<", viewMapMain model, txt ">" ]

        botRow =
            row [ width fill ] [ txt "v" ]
    in
    el [] <|
        row []
            [ column []
                [ topRow
                , midRow
                , botRow
                ]
            , playerProps model
            ]


playerProps : Model -> Element Msg
playerProps model =
    none


viewMapMain : Model -> Element Msg
viewMapMain model =
    el [ width (px 500), height (px 500) ] none


txt : String -> Element Msg
txt s =
    let
        borderAttrs =
            [ Border.color (Element.rgb 0 0 0)
            , Border.width 2
            , Border.solid
            ]
    in
    el (borderAttrs ++ [ width fill, height fill, Font.center ]) (text s)


wut : Attribute Msg
wut =
    explain Debug.todo
