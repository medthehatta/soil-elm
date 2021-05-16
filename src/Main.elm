module Main exposing (..)

import Browser exposing (element)
import Debug exposing (log)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as Attrs
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Task exposing (Task)


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        msg : Msg
        msg =
            case testPayload of
                Ok data ->
                    GotPayload data

                Err err ->
                    GotBadPayload err
    in
    update msg Loading


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--- TYPES


type Msg
    = Noop
    | GotPayload MapPayload
    | GotBadPayload D.Error
    | Clicked String


type Model
    = Loading
    | MapDisplay MapPayload
    | DebugString String
    | DecodeErrorDisplay D.Error


testPayloadJSON : String
testPayloadJSON =
    """
{"tile": "tile.png",
 "pois": [{"name": "Market1", "coords": {"x": 200, "y": 100}}
         ,{"name": "Market2", "coords": {"x": 390, "y": 440}}
         ]}
"""


testPayload =
    D.decodeString mapDecoder testPayloadJSON


type alias MapPayload =
    { tile : String
    , pois : List POI
    }


type alias POI =
    { name : String
    , coords : Coords
    }


type alias Coords =
    { x : Int, y : Int }


coordsFromPair : Int -> Int -> Coords
coordsFromPair x y =
    { x = x, y = y }


mapDecoder : D.Decoder MapPayload
mapDecoder =
    let
        dCoords : D.Decoder Coords
        dCoords =
            D.succeed Coords
                |> required "x" D.int
                |> required "y" D.int

        dPOI : D.Decoder POI
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
        ( GotPayload payload, _ ) ->
            ( MapDisplay payload, Cmd.none )

        ( GotBadPayload err, _ ) ->
            ( DecodeErrorDisplay err, Cmd.none )

        ( Clicked name, _ ) ->
            ( DebugString name, Cmd.none )

        ( Noop, _ ) ->
            ( model, Cmd.none )



--- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            layout [] (txt "Loading...")

        MapDisplay payload ->
            layout [] (uiView payload)

        DecodeErrorDisplay err ->
            layout [] (el [] (text (D.errorToString err)))

        DebugString err ->
            layout [] (el [] (text err))


uiView : MapPayload -> Element Msg
uiView payload =
    column [ width fill ]
        [ viewMapWithBorderNav payload
        ]


viewMapWithBorderNav : MapPayload -> Element Msg
viewMapWithBorderNav payload =
    let
        topRow =
            row [ width fill ] [ txt "^" ]

        midRow =
            row [ width fill ] [ txt "<", viewMap payload, txt ">" ]

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
            , playerProps payload
            ]


playerProps : MapPayload -> Element Msg
playerProps payload =
    none


viewMap : MapPayload -> Element Msg
viewMap payload =
    let
        w =
            500

        h =
            500

        baseAttrs =
            [ width (px w), height (px h), Background.image payload.tile ]

        iconAt name { x, y } =
            htmlIcon "room"
                |> html
                |> el
                    [ moveRight (toFloat x)
                    , moveDown (toFloat y)
                    , Font.color (rgb 1 0 0)
                    , pointer
                    , onClick (Clicked name)
                    ]

        points =
            List.map (\poi -> iconAt poi.name poi.coords) payload.pois
    in
    el baseAttrs (absolute points)


absolute : List (Element Msg) -> Element Msg
absolute elements =
    el (List.map (\el -> inFront el) elements) none


txt : String -> Element Msg
txt s =
    let
        borderAttrs =
            [ Border.color (rgb 0 0 0)
            , Border.width 2
            , Border.solid
            ]
    in
    el (borderAttrs ++ [ width fill, height fill, Font.center ]) (text s)


wut : Attribute Msg
wut =
    explain Debug.todo


htmlIcon : String -> Html Msg
htmlIcon name =
    Html.span [ Attrs.class "material-icons" ] [ Html.text name ]
