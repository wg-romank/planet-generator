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
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { theta = 0
      , paused = False
      , viewportHeight = 0
      , viewportWidth = 0
      , offset = 0
      , width = 400
      , height = 400
      , noiseParams = emptyNoiseParams
      , meshes = makeCube emptyNoiseParams
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




computeViewportSize : Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        ratio = 16.0 / 9.0
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
            in
            ( { model | noiseParams = newParams, meshes = makeCube newParams }, Cmd.none )


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
        style "width" "80%",
        style "height" "4em",
        style "align" "center"
    ] [
        div [ style "align" "left" ] [text label],
    input [
        type_ "range",
        H.min (String.fromFloat minValue),
        H.max (String.fromFloat maxValue),
        H.step "0.01",
        value <| String.fromFloat actualValue,
        onInput (\x -> UpdateParams (up x))
    ] [ ] ]

intSlider: String -> (String -> UpdateParams) -> Int -> Int -> Int -> Html Msg
intSlider label up minValue maxValue actualValue = 
    div [
        style "width" "80%",
        style "height" "4em",
        style "align" "center"
    ] [
        div [ style "align" "left" ] [text label],
    input [
        type_ "range",
        H.min (String.fromInt minValue),
        H.max (String.fromInt maxValue),
        H.step "1",
        value <| String.fromInt actualValue,
        onInput (\x -> UpdateParams (up x))
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
        [ div [] [
            WebGL.toHtmlWith [ WebGL.depth 1, WebGL.antialias, WebGL.stencil 0 ]
            [ style "backgroundColor" "#FFFFFF",
              style "display" "block",
              style "align" "center",
              style "width" "95%",
              width model.width,
              height model.height ] (List.map (draw model.theta) model.meshes),
          div [
            style "top" (String.fromInt model.height ++ "px"),
            style "align" "center",
            style "position" "absolute",
            style "width" "95%"
          ] [ 
            -- intSlider "seed" UpdateSeed 1 100 model.noiseParams.seed,
            slider "baseRoughness" UpdateBaseRoughness 1 5 model.noiseParams.baseRoughness,
            slider "roughness" UpdateRoughness 0 1 model.noiseParams.roughness,
            slider "persistance" UpdatePersistance 0 1 model.noiseParams.persistance,
            slider "strength" UpdateStrength 0 1 model.noiseParams.strength,
            slider "minValue" UpdateMinValue -0.3 1 model.noiseParams.minValue,
            intSlider "numLayers" UpdateNumLayers 1 8 model.noiseParams.numLayers
          ] ]
         ]
        ]
    }
