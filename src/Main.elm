module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import WebGL
import Shaders exposing (..)


main : Program () Model Msg
main = Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { theta: Float }


init : flags -> (Model, Cmd Msg)
init flags =
    ( {theta = 0 }, Cmd.none)


type Msg
    = Delta (Float)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Delta f ->
            let
                th = model.theta
            in
            ( { model | theta = th + f / 5000 }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta Delta


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [ div []
            [ text "New Document" ],
          WebGL.toHtml [
              style "backgroundColor" "black",
              width 400,
              height 400
          ] (drawCube model.theta)
      ]
    }


