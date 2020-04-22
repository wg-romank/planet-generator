module Shaders exposing (makeCube, draw, Vertex)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2)
import Math.Matrix4 as Mat4 exposing (Mat4)

import Simplex
import WebGL exposing (Mesh)
import WebGL.Settings exposing (back)
import WebGL.Settings.DepthTest as DepthTest

import NoiseParameters exposing (NoiseParameters)

type alias Vertex =
    { position : Vec3
    , normal: Vec3
    }

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
            vec4 pos = perspective * rotation * vec4(position, 1);
            gl_Position = pos;

            vec3 ambientLight = vec3(0.3, 0.3, 0.3);
            vec3 directionalLightColor = vec3(1, 1, 1);
            vec3 directionalVector = normalize(vec3(-0.85, -0.8, -0.75));

            vec4 transformedNormal = normalMatrix * vec4(normal, 0.0);

            float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
            vLighting = ambientLight + (directionalLightColor * directional);

            float height = max(0.2, length(pos.xyz) - length(transformedNormal.xyz)) / maxHeight;
            heightColor = vec3(height * height, (1.0 - height) / (2.0), (1.0 - height * height) / 2.0);
        }
    |]

fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vLighting;
        varying vec3 heightColor;

        void main() {
            gl_FragColor = vec4(heightColor * vLighting, 1);
        }
    |]

face: (Float -> Float -> Float -> Float) -> Int -> Vec3 -> (Float, Mesh Vertex)
face noise res direction =
    let
        axisA = vec3 (Vec3.getY direction) (Vec3.getZ direction) (Vec3.getX direction)
        axisB = Vec3.cross direction axisA
        vertexes =
            List.range 0 (res - 1) |>
                List.map (\y -> List.range 0 (res - 1) |> List.map (\x ->
                    let
                        floatX = toFloat x
                        floatY = toFloat y
                        floatRes = toFloat res - 1
                        percent = vec2 (floatY / floatRes) (floatX / floatRes)
                        pointOnUniSphere = direction
                            |> Vec3.add (Vec3.scale (Vec2.getX percent * 2 - 1) axisA)
                            |> Vec3.add (Vec3.scale (Vec2.getY percent * 2 - 1) axisB)
                            |> Vec3.normalize
                        noiseV = noise (Vec3.getX pointOnUniSphere) (Vec3.getY pointOnUniSphere) (Vec3.getZ pointOnUniSphere)
                        point = Vec3.scale (1 + noiseV) pointOnUniSphere
                    in
                        { position = point, normal = pointOnUniSphere }
                        
                    )
                ) |> List.concat
        maxHeight = List.map (\v -> Vec3.sub v.position v.normal |> Vec3.length) vertexes |> List.maximum |> Maybe.withDefault 0
        indices = List.range 0 (res - 1) |>
            List.map (\y -> List.range 0 (res - 1) |> List.map (\x -> 
                let
                    vertexId = y * res + x
                in
                    if x < res - 1 && y < res - 1 then
                        [
                            (vertexId, vertexId + res + 1, vertexId + res),
                            (vertexId, vertexId + 1, vertexId + res + 1)
                        ]
                    else []
            ) ) |> List.concat |> List.concat
    in
        (maxHeight, WebGL.indexedTriangles vertexes indices)
        

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

uniforms: Float -> Float -> Float -> Float -> Uniforms
uniforms width height maxHeight theta =
    let
        rotation = Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            Mat4.identity
            -- (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
        perspectiveP = perspective width height 0 0
        normalTransform = 
            Mat4.mul rotation perspectiveP
            |> Mat4.inverse
            |> Maybe.withDefault Mat4.identity
            |> Mat4.transpose
        -- rotation = Mat4.identity
        -- normalTransform = Mat4.identity
    in
    { rotation = rotation, normalMatrix = normalTransform, maxHeight = maxHeight, perspective = perspectiveP }

draw: Float -> Float -> Float -> Float -> Mesh Vertex -> WebGL.Entity
draw width height maxHeight theta mesh = WebGL.entityWith
    [ WebGL.Settings.cullFace back
    , DepthTest.default 
    ] vertexShader fragmentShader mesh (uniforms width height maxHeight theta)

makeCube: NoiseParameters -> (Float, List (Mesh Vertex))
makeCube noiseParams = 
    let
        noise = Simplex.noise3d (Simplex.permutationTableFromInt noiseParams.seed)  -- [0, 1]
        initialParams = { value = 0.0, frequency = noiseParams.baseRoughness, amplidute = 1 }
        noiseFunc = \x y z ->
            List.foldl ( \_ acc ->
                let
                    noiseMinusOneOne = noise (acc.frequency * x) (acc.frequency * y) (acc.frequency * z)
                    noiseZeroOne = (noiseMinusOneOne + 1) * 0.5
                    -- noiseZeroOne = noiseMinusOneOne
                    v = noiseZeroOne * acc.amplidute
                    newFrequency = acc.frequency * noiseParams.roughness
                    newAmp = acc.amplidute * noiseParams.persistance
                in
                    { value = acc.value + v, frequency = newFrequency, amplidute = newAmp }
            ) initialParams (List.range 1 noiseParams.numLayers)
            |> \f -> max 0 ((f.value * noiseParams.strength - noiseParams.minValue) / toFloat noiseParams.numLayers) -- [0, ???]
    in
    let
        (a, b) =
            [ vec3 1 0 0,
              vec3 0 1 0,
              vec3 0 0 1,
              vec3 -1 0 0,
              vec3 0 -1 0,
              vec3 0 0 -1 ]
                |> List.map (face noiseFunc noiseParams.resolution) |> List.unzip
    in
        (a |> List.maximum |> Maybe.withDefault 0, b)