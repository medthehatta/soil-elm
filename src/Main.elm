port module Main exposing (..)

import Browser exposing (element)
import Canvas
import Debug exposing (log)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D


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


port canvasClick : (D.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        clickPosDecoder =
            D.map2 (\x -> \y -> ( x, y )) (D.field "x" D.int) (D.field "y" D.int)

        readPos : D.Value -> Msg
        readPos click =
            let
                found =
                    D.decodeValue clickPosDecoder click
            in
            case found of
                Ok x ->
                    CanvasClicked x

                Err _ ->
                    Noop
    in
    canvasClick readPos


type Msg
    = Noop
    | CanvasClicked ( Int, Int )


type Model
    = Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( CanvasClicked _, _ ) ->
            ( model, Cmd.none )


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
            row [ width fill ] [ txt "<", mapCanvas model, txt ">" ]

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


mapCanvas : Model -> Element Msg
mapCanvas model =
    let
        w =
            500

        h =
            500
    in
    el [ width (px w), height (px h), htmlAttribute (Attrs.id "mapCanvas") ]
        (html <| Canvas.toHtml ( w, h ) [] [])


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
