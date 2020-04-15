module Shaders exposing (..)

import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Matrix4 as Mat4 exposing (Mat4)

import Simplex exposing (PermutationTable)
import WebGL exposing (Mesh)

type alias Vertex = {
        position : Vec3
    }

permTable: PermutationTable
permTable = Simplex.permutationTableFromInt 42

vertexShader = 
    [glsl|
        attribute vec3 position;
        uniform mat4 rotation;

        void main() {
            gl_Position = rotation * vec4(position, 2);
        }
    |]

fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 vcolor;
        void main() {
            gl_FragColor = vcolor;
        }
    |]

face: Int -> Vec3 -> Mesh Vertex
face res direction =
    let
        noise = Simplex.fractal3d { scale = 4.0, steps = 4, stepSize = 0.8, persistence = 2.0 } permTable
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
                        point = Vec3.scale (1 + (noiseV + 1) / 2) pointOnUniSphere
                    in
                        { position = point }
                        
                    )
                ) |> List.concat
        indices = List.range 0 (res - 1) |>
            List.map (\y -> List.range 0 (res - 1) |> List.map (\x -> 
                let
                    vertexId = y * res + x
                in
                    if x < res - 1 && y < res - 1 then
                        [
                            (vertexId, vertexId + res + 1, vertexId + res),
                            (vertexId, vertexId + 1, vertexId + res + 1)
                            -- (vertexId + res + 1, vertexId + 1, vertexId),
                            -- (vertexId, vertexId + res, vertexId + res + 1)
                        ]
                    else []
            ) ) |> List.concat |> List.concat
    in
        WebGL.indexedTriangles vertexes indices
        

type alias Uniforms = {
        rotation: Mat4,
        vcolor: Vec4
    }

uniforms: Float -> Uniforms
uniforms theta =
    { rotation = Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (2 * theta) (vec3 1 0 0)),
      vcolor = vec4 0.5 0.5 1 1 }

draw: Float -> Mesh Vertex -> WebGL.Entity
draw theta mesh = WebGL.entityWith [] vertexShader fragmentShader mesh (uniforms theta)

drawFace: Float -> Vec3 -> WebGL.Entity
drawFace theta dir = draw theta  (face 10 dir)

drawCube: Float -> List WebGL.Entity
drawCube theta =
    [ vec3 1 0 0,
      vec3 0 1 0,
      vec3 0 0 1,
      vec3 -1 0 0,
      vec3 0 -1 0,
      vec3 0 0 -1 ]
        |> List.map (drawFace theta)

