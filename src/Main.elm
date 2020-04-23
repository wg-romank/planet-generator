module Main exposing (Model, Msg, init, subscriptions, update, view, main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import Task
import WebGL

import NoiseParameters exposing (..)
import Shaders exposing (..)
import Mesh exposing (..)

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
    , noiseParams : List NoiseParameters
    , meshes: List (WebGL.Mesh Vertex)
    , maxHeight: Float
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        noiesParams = [emptyNoiseParams]
        (maxHeight, cubeMesh) = makeCube noiesParams
    in
    ( { theta = 0
      , paused = False
      , viewportHeight = 0
      , viewportWidth = 0
      , offset = 0
      , width = 240
      , height = 240
      , noiseParams = noiesParams
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
    | UpdateParams Int UpdateParams
    | AddFilter
    | RemoveFilter




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
        UpdateParams idx paramsUpdate ->
            let
                newParams = updateParameter idx model.noiseParams paramsUpdate
                (newHeight, newCube) = makeCube newParams
            in
            ( { model | noiseParams = newParams, meshes = newCube, maxHeight = newHeight }, Cmd.none )
        AddFilter ->
            let prevParamsList = model.noiseParams in
            ( { model | noiseParams = emptyNoiseParams :: prevParamsList }, Cmd.none )
        RemoveFilter ->
            let prevParamsList = model.noiseParams in
            ( { model | noiseParams = prevParamsList |> List.drop 1 }, Cmd.none)


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

slider: Int -> String -> (String -> UpdateParams) -> Float -> Float -> Float -> Html Msg
slider idx label up minValue maxValue actualValue = 
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
        onInput (\x -> UpdateParams idx (up x)),
        style "width" "100%"
    ] [ ] ]

intSlider: Int -> String -> (String -> UpdateParams) -> Int -> Int -> Int -> Html Msg
intSlider idx label up minValue maxValue actualValue = 
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
        onInput (\x -> UpdateParams idx (up x)),
        style "width" "100%"
    ] [ ] ]


makeParamControl: Int -> NoiseParameters -> Html Msg
makeParamControl idx noiseParams = 
    div [
      -- style "top" (String.fromInt model.height ++ "px"),
      style "align" "center",
      style "position" "absolute",
      style "width" "100%"
    ] [ 
      intSlider idx "Seed" UpdateSeed 1 100 noiseParams.seed,
      slider idx "Base Roughness" UpdateBaseRoughness 1 5 noiseParams.baseRoughness,
      slider idx "Roughness" UpdateRoughness 0 1 noiseParams.roughness,
      slider idx "Persistance" UpdatePersistance 0 1 noiseParams.persistance,
      slider idx "Strength" UpdateStrength 0 2 noiseParams.strength,
      slider idx "Min Value" UpdateMinValue -0.3 1 noiseParams.minValue,
      intSlider idx "Num Layers" UpdateNumLayers 1 8 noiseParams.numLayers
    ]

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
        [ 
            div [ style "align" "center", style "position" "absolute" ] 
            [
                WebGL.toHtmlWith
                    [ WebGL.depth 1, WebGL.antialias, WebGL.stencil 0 ]
                    [ 
                        style "display" "block",
                        style "align" "center",
                        width model.width,
                        height model.height
                    ]
                    (List.map 
                        (draw (toFloat model.width) (toFloat model.height) model.maxHeight model.theta)
                        model.meshes),
                div [] [
                    button [
                        Html.Events.onClick AddFilter,
                        style "color" "white"
                    ] [ text "+"],
                    button [
                        Html.Events.onClick RemoveFilter,
                        style "color" "white"
                    ] [ text "-"],
                    div [] (List.indexedMap makeParamControl model.noiseParams)
                ]
            ]
        ]
        ]
    }
