module Shaders exposing (draw)

import Math.Vector3 as Vec3 exposing (vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh)
import WebGL.Settings exposing (back)
import WebGL.Settings.DepthTest as DepthTest

import NoiseParameters exposing (NoiseParameters)
import Mesh exposing (..)


vertexShader = 
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 rotation;
        uniform mat4 normalMatrix;
        uniform mat4 perspective;
        uniform float maxHeight;
        varying vec3 vLighting;
        varying vec3 heightColor;

        void main() {
            vec4 pos = rotation * vec4(position, 2);
            gl_Position = pos;

            vec3 ambientLight = vec3(0.3, 0.3, 0.3);
            vec3 directionalLightColor = vec3(1, 1, 1);
            vec3 directionalVector = normalize(vec3(-0.85, -0.8, -0.75));

            vec4 transformedNormal = normalMatrix * vec4(normal, 0.0);

            float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
            vLighting = ambientLight + (directionalLightColor * directional);

            float height = max(0.0, length(pos.xyz) - length(transformedNormal.xyz)) / maxHeight;
            heightColor = vec3(height, (1.0 - height * height), (1.0 - height));
        }
    |]

fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vLighting;
        varying vec3 heightColor;

        void main() {
            gl_FragColor = vec4(vLighting * heightColor.x, 1);
        }
    |]


type alias Uniforms = 
    { rotation: Mat4
    , perspective: Mat4
    , normalMatrix: Mat4
    , maxHeight: Float
    }

perspective : Float -> Float -> Float -> Float -> Mat4
perspective width height x y =
    let
        eye =
            vec3 (0.5 - x / width) -(0.5 - y / height) 1
                |> Vec3.normalize
                |> Vec3.scale 6
    in
        Mat4.mul
            (Mat4.makePerspective 45 (width / height) 0.01 100)
            (Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j)

makeUniforms: Float -> Float -> Float -> Float -> Uniforms
makeUniforms width height maxHeight theta =
    let
        rotation = Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            Mat4.identity
            -- (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
        perspectiveP = perspective width height 0 0
        normalTransform = 
            rotation
            |> Mat4.inverse
            |> Maybe.withDefault Mat4.identity
            |> Mat4.transpose
        -- rotation = Mat4.identity
        -- normalTransform = Mat4.identity
    in
    { rotation = rotation, normalMatrix = normalTransform, maxHeight = maxHeight, perspective = perspectiveP }

draw: Float -> Float -> Float -> Float -> Mesh Vertex -> WebGL.Entity
draw width height maxElevation theta mesh =
    WebGL.entityWith
        [ WebGL.Settings.cullFace back, DepthTest.default]
        vertexShader
        fragmentShader
        mesh
        (makeUniforms width height maxElevation theta)