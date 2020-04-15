module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import Shaders exposing (..)
import Task
import WebGL


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { theta : Float
    , paused : Bool
    , viewportHeight : Int
    , viewportWidth : Int
    , width : Int
    , height : Int
    , offset : Int
    , noiseParams : NoiseParameters
    , res : Int
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { theta = 0
      , paused = False
      , viewportHeight = 0
      , viewportWidth = 0
      , offset = 0
      , width = 600
      , height = 800
      , noiseParams = emptyNoiseParams
      , res = 10
      }
    , Cmd.batch
        [ Task.perform ViewPortLoaded Browser.Dom.getViewport
        ]
    )


type Msg
    = Delta Float
    | Paused
    | Resumed
    | ViewPortLoaded Viewport
    | UpdateParams UpdateParams


type UpdateParams = UpdateScale String | UpdatePeriod String | UpdatePersistance String | UpdateOctaves String


computeViewportSize : Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph =
            viewport.viewport.height

        ratio =
            toFloat model.height / toFloat model.width

        vpw =
            vph / ratio

        offset =
            (viewport.viewport.width - vpw) / 2.0 |> round
    in
    { model
        | viewportWidth = Basics.round vpw
        , viewportHeight = Basics.round vph
        , offset = offset
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delta f ->
            let
                th = model.theta
            in
            ( { model | theta = th + f / 5000 }, Cmd.none )
        Paused ->
            ( { model | paused = True }, Cmd.none )
        Resumed ->
            ( { model | paused = False }, Cmd.none )
        ViewPortLoaded viewport ->
            ( computeViewportSize viewport model, Cmd.none )
        UpdateParams params ->
            let
                prevParams = model.noiseParams
                newParams = case params of 
                    UpdateScale scale ->
                        { prevParams | scale = String.toFloat scale |> Maybe.withDefault prevParams.scale }
                    UpdatePeriod period ->
                        { prevParams | period = String.toFloat period |> Maybe.withDefault prevParams.period }
                    UpdatePersistance persistance ->
                        { prevParams | persistance = String.toFloat persistance |> Maybe.withDefault prevParams.persistance }
                    UpdateOctaves octaves ->
                        { prevParams | octaves = String.toInt octaves |> Maybe.withDefault prevParams.octaves }
            in
            ( { model | noiseParams = newParams }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused then
            Sub.none

          else
            onAnimationFrameDelta Delta
        , Browser.Events.onVisibilityChange
            (\v ->
                case v of
                    Browser.Events.Hidden ->
                        Paused

                    Browser.Events.Visible ->
                        Resumed
            )
        ]

slider: String -> (String -> UpdateParams) -> Float -> Float -> Float -> Html Msg
slider label up minValue maxValue actualValue = 
    input [
        type_ "range",
        H.min (String.fromFloat minValue),
        H.max (String.fromFloat maxValue),
        H.step "0.1",
        value <| String.fromFloat actualValue,
        onInput (\x -> UpdateParams (up x))
    ] [ text label ]

intSlider: String -> (String -> UpdateParams) -> Int -> Int -> Int -> Html Msg
intSlider label up minValue maxValue actualValue = 
    input [
        type_ "range",
        H.min (String.fromInt minValue),
        H.max (String.fromInt maxValue),
        H.step "1",
        value <| String.fromInt actualValue,
        onInput (\x -> UpdateParams (up x))
    ] [ text label ]

view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [ WebGL.toHtml
            [ style "top" "0px"
            , style "left" (String.fromInt model.offset ++ "px")
            , style "position" "absolute"
            , style "backgroundColor" "black"
            , style "width" (String.fromInt model.viewportWidth ++ "px")
            , style "height" (String.fromInt model.viewportWidth ++ "px")
            , style "display" "block"
            , width model.width
            , height model.height
            ]
            (drawCube model.res model.theta model.noiseParams)
        , div [
            style "top" (String.fromInt model.height ++ "px"),
            style "position" "absolute",
            style "width" (String.fromInt model.width ++ "px"),
            style "left" (String.fromInt model.offset ++ "px")
        ] [ 
            slider "scale" UpdateScale 0.3 4 model.noiseParams.scale,
            slider "period" UpdatePeriod 0.1 4 model.noiseParams.period,
            slider "persistance" UpdatePersistance 0 1 model.noiseParams.persistance,
            slider "octaves" UpdateOctaves 1 8 model.noiseParams.persistance
         ]
        ]
    }
