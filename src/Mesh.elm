module Mesh exposing (..)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2)
import WebGL exposing (Mesh)
import Simplex

import NoiseParameters exposing (..)

type alias Vertex =
    { position : Vec3
    , normal: Vec3
    }

type alias Noise3d = Float -> Float -> Float -> Float

face: Noise3d -> Int -> Vec3 -> (Float, Mesh Vertex)
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

makeNoiseFunc: Noise3d -> NoiseParameters -> Noise3d
makeNoiseFunc noise noiseParams x y z =
    let
        initialParams = { value = 0.0, frequency = noiseParams.baseRoughness, amplidute = 1 }
    in
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


makeCube: List NoiseParameters -> (Float, List (Mesh Vertex))
makeCube noiseParamsList = 
    let
        noiseFunctions = List.map (\p -> Simplex.noise3d (Simplex.permutationTableFromInt p.seed)) noiseParamsList
        noiseFilters = List.map2 makeNoiseFunc noiseFunctions noiseParamsList
        noiseFunc = \x y z -> List.foldl (\f acc -> (acc + f x y z) / 2.0) 0 noiseFilters
        firstParams = List.head noiseParamsList |> Maybe.withDefault emptyNoiseParams
        res = firstParams.resolution
    in
    let
        (a, b) =
            [ vec3 1 0 0,
              vec3 0 1 0,
              vec3 0 0 1,
              vec3 -1 0 0,
              vec3 0 -1 0,
              vec3 0 0 -1 ]
                |> List.map (face noiseFunc res)
                |> List.unzip
        maxElevation = a |> List.maximum |> Maybe.withDefault 0
    in
        (maxElevation, b)