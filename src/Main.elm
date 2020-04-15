module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta)
import WebGL
import Task
import Shaders exposing (..)


main : Program {} Model Msg
main = Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { theta: Float, paused: Bool, viewportHeight: Int, viewportWidth: Int, width: Int, height: Int, offset: Int, noiseParams: NoiseParameters, res: Int }


init : {} -> (Model, Cmd Msg)
init _ =
    ( {theta = 0,
       paused = False,
       viewportHeight = 0,
       viewportWidth = 0,
       offset = 0,
       width = 600,
       height = 800,
       noiseParams = emptyNoiseParams,
       res = 10
      }, Cmd.batch [
        Task.perform ViewPortLoaded Browser.Dom.getViewport
    ])


type Msg
    = Delta (Float)
    | Paused
    | Resumed
    | ViewPortLoaded (Viewport)

computeViewportSize: Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        vpm = viewport.viewport.height / toFloat model.height
        ratio = toFloat model.height / toFloat model.width
        vpw = vph / ratio
        offset = (viewport.viewport.width - vpw) / 2.0 |> round
    in 
    {model |
        viewportWidth = Basics.round vpw,
        viewportHeight = Basics.round vph,
        offset = offset }


update : Msg -> Model -> (Model, Cmd Msg)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        if model.paused then Sub.none
        else onAnimationFrameDelta Delta,
        Browser.Events.onVisibilityChange (\v ->
            case v of
              Browser.Events.Hidden -> Paused
              Browser.Events.Visible -> Resumed)
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [
          WebGL.toHtml [
              style "top" "0",
              style "left" (String.fromInt model.offset ++ "px"),
              style "position" "absolute",
              style "backgroundColor" "black",
              style "width" (String.fromInt model.viewportWidth ++ "px"),
              style "height" (String.fromInt model.viewportWidth ++ "px"),
              style "display" "block",
              width model.viewportWidth,
              height model.viewportHeight
          ] (drawCube model.res model.theta model.noiseParams)
      ]
    }


