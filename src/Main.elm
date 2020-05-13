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
        UpdateParams idx paramsUpdate ->
            let
                newParams = updateParameter idx model.noiseParams paramsUpdate
                (newHeight, newCube) = makeCube newParams
            in
            ( { model | noiseParams = newParams, meshes = newCube, maxHeight = newHeight }, Cmd.none )
        AddFilter ->
            let
                newParams = emptyNoiseParams :: model.noiseParams
                (newHeight, newCube) = makeCube newParams
            in
            ( { model | noiseParams = newParams, meshes = newCube, maxHeight = newHeight }, Cmd.none )
        RemoveFilter ->
            let
                newParams = model.noiseParams |> List.drop 1
                (newHeight, newCube) = makeCube newParams
            in
            ( { model | noiseParams = newParams, maxHeight = newHeight, meshes = newCube }, Cmd.none)


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

gSlider: String -> Int -> String -> (String -> UpdateParams)  -> (a -> String) -> a -> a -> a -> Html Msg
gSlider step idx label up toString minValue maxValue actualValue =
    div [
        class "control-element"
    ] [
        div [ ]
        [
            output [
                style "color" "white"
            ] [text label]
        ],
        div [ ]
        [
            input [
                type_ "range",
                style "align" "auto",
                H.min (toString minValue),
                H.max (toString maxValue),
                H.step step,
                value <| toString actualValue,
                onInput (\x -> UpdateParams idx (up x))
            ] [ ]
        ],
        div [ ]
        [
            output [
                style "color" "white"
            ] [ text (toString actualValue) ]
        ]
    ]


slider: Int -> String -> (String -> UpdateParams) -> Float -> Float -> Float -> Html Msg
slider idx label up minValue maxValue actualValue = 
    gSlider  "0.01" idx label up String.fromFloat minValue maxValue actualValue

intSlider: Int -> String -> (String -> UpdateParams) -> Int -> Int -> Int -> Html Msg
intSlider idx label up minValue maxValue actualValue = 
    gSlider "1" idx label up String.fromInt minValue maxValue actualValue


makeFilterControl: Int -> NoiseParameters -> Html Msg
makeFilterControl idx noiseParams = 
    div [
        class "control"
    ] [ 
      intSlider idx "Seed" UpdateSeed 1 100 noiseParams.seed,
      slider idx "Base Roughness" UpdateBaseRoughness 1 5 noiseParams.baseRoughness,
      slider idx "Roughness" UpdateRoughness 0 1 noiseParams.roughness,
      slider idx "Persistance" UpdatePersistance 0 1 noiseParams.persistance,
      slider idx "Strength" UpdateStrength 0 4 noiseParams.strength,
      slider idx "Min Value" UpdateMinValue 0 1 noiseParams.minValue,
      intSlider idx "Num Layers" UpdateNumLayers 1 8 noiseParams.numLayers,
      intSlider idx "Resolution" UpdateResolution 10 100 noiseParams.resolution
    ]

view : Model -> Browser.Document Msg
view model =
    { title = "Planet generator"
    , body = [
            div [ 
                class "main"
            ] 
            [
                WebGL.toHtmlWith
                    [ WebGL.depth 1, WebGL.antialias, WebGL.stencil 0 ]
                    [ 
                        width model.width,
                        height model.height
                    ]
                    (List.map 
                        (draw (toFloat model.width) (toFloat model.height) model.maxHeight model.theta)
                        model.meshes),

                div [ class "control-element" ] 
                [
                    button [ Html.Events.onClick AddFilter ] [ text "+"],
                    div [
                        style "color" "white",
                        style "font-size" "2em"
                    ] [text "Filters"],
                    button [ Html.Events.onClick RemoveFilter ] [ text "-"] ],
                div [ class "filters" ] (List.indexedMap makeFilterControl model.noiseParams)
            ]
        ]
    }
