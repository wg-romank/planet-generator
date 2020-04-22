module Main exposing (Model, Msg, init, subscriptions, update, view, main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import Task
import WebGL

import Shaders exposing (..)
import NoiseParameters exposing (..)

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
    , meshes: List (WebGL.Mesh Vertex)
    , maxHeight: Float
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        (maxHeight, cubeMesh) = makeCube emptyNoiseParams
    in
    ( { theta = 0
      , paused = False
      , viewportHeight = 0
      , viewportWidth = 0
      , offset = 0
      , width = 240
      , height = 240
      , noiseParams = emptyNoiseParams
      , meshes = cubeMesh
      , maxHeight = maxHeight
      }
    , Cmd.batch [ Task.perform ViewPortLoaded Browser.Dom.getViewport ]
    )


type Msg
    = Delta Float
    | Paused
    | Resumed
    | ViewPortLoaded Viewport
    | UpdateParams UpdateParams




computeViewportSize : Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        ratio = 4.0 / 3.0
        vpw = vph / ratio
        offset = (viewport.viewport.width - vpw) / 2.0 |> round
    in
    { model | viewportWidth = Basics.round vpw, viewportHeight = Basics.round vph, offset = offset }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delta f -> ( { model | theta = model.theta + f / 5000 }, Cmd.none )
        Paused -> ( { model | paused = True }, Cmd.none )
        Resumed -> ( { model | paused = False }, Cmd.none )
        ViewPortLoaded viewport -> ( computeViewportSize viewport model, Cmd.none )
        UpdateParams paramsUpdate ->
            let
                newParams = updateParameter model.noiseParams paramsUpdate
                (newHeight, newCube) = makeCube newParams
            in
            ( { model | noiseParams = newParams, meshes = newCube, maxHeight = newHeight }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
          if model.paused then Sub.none
          else onAnimationFrameDelta Delta,
          Browser.Events.onVisibilityChange
            (\v ->
                case v of
                    Browser.Events.Hidden -> Paused
                    Browser.Events.Visible -> Resumed
            )
        ]

slider: String -> (String -> UpdateParams) -> Float -> Float -> Float -> Html Msg
slider label up minValue maxValue actualValue = 
    div [
        style "width" "100%",
        style "height" "4em"
        -- style "align" "center"
    ] [
        div [ style "align" "left", style "color" "white" ] [text label],
    input [
        type_ "range",
        H.min (String.fromFloat minValue),
        H.max (String.fromFloat maxValue),
        H.step "0.01",
        value <| String.fromFloat actualValue,
        onInput (\x -> UpdateParams (up x)),
        style "width" "100%"
    ] [ ] ]

intSlider: String -> (String -> UpdateParams) -> Int -> Int -> Int -> Html Msg
intSlider label up minValue maxValue actualValue = 
    div [
        style "width" "100%",
        style "height" "4em"
        -- style "align" "center"
    ] [
        div [ style "align" "left", style "color" "white" ] [text label],
    input [
        type_ "range",
        H.min (String.fromInt minValue),
        H.max (String.fromInt maxValue),
        H.step "1",
        value <| String.fromInt actualValue,
        onInput (\x -> UpdateParams (up x)),
        style "width" "100%"
    ] [ ] ]

view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body = [
        div [
            style "top" "0px",
            style "left" (String.fromInt model.offset ++ "px"),
            style "position" "absolute",
            style "width" (String.fromInt model.viewportWidth ++ "px"),
            style "height" (String.fromInt model.viewportHeight ++ "px")
        ]
        [ div [ style "align" "center", style "position" "absolute" ] [
            WebGL.toHtmlWith [ WebGL.depth 1, WebGL.antialias, WebGL.stencil 0 ]
            [ 
              style "display" "block",
              style "align" "center",
              width model.width,
              height model.height ] (List.map (draw (toFloat model.width) (toFloat model.height) model.maxHeight model.theta) model.meshes),
          div [
            -- style "top" (String.fromInt model.height ++ "px"),
            style "align" "center",
            style "position" "absolute",
            style "width" "100%"
          ] [ 
            intSlider "Seed" UpdateSeed 1 100 model.noiseParams.seed,
            slider "Base Roughness" UpdateBaseRoughness 1 5 model.noiseParams.baseRoughness,
            slider "Roughness" UpdateRoughness 0 1 model.noiseParams.roughness,
            slider "Persistance" UpdatePersistance 0 1 model.noiseParams.persistance,
            slider "Strength" UpdateStrength 0 1 model.noiseParams.strength,
            slider "Min Value" UpdateMinValue -0.3 1 model.noiseParams.minValue,
            intSlider "Num Layers" UpdateNumLayers 1 8 model.noiseParams.numLayers
          ] ]
         ]
        ]
    }
