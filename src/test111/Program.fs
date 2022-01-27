open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open FSharp.Data.Adaptive

module Shader =
    open FShade


    let map =
        samplerCube {
            texture uniform?CubeMap
            filter Filter.MinMagLinear
        }

    let cubeMap (v : Effects.Vertex) =
        fragment {
            let vp = uniform.ProjTrafoInv * v.pos
            let vd = vp.XYZ / vp.W
            let dir = uniform.ViewTrafoInv * V4d(vd, 0.0) |> Vec.xyz |> Vec.normalize
            let phi0 = atan2 dir.Y dir.X
            let theta0 = asin dir.Z

            let c = v.pos.XY / v.pos.W

            let phi = phi0 + c.X * Constant.Pi
            let theta = theta0 + c.Y * Constant.PiHalf


            let dir = 
                V3d(
                    cos phi * cos theta,
                    sin phi * cos theta,
                    sin theta
                )



            return map.SampleLevel(dir, 0.0)
        }

[<EntryPoint;STAThread>]
let main argv = 
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(1)

    let quadGeometry =
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = ([|0;1;2; 0;2;3|] :> Array),
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO |] :> Array
                    DefaultSemantic.Colors, [| C4b.Red; C4b.Green; C4b.Blue; C4b.Yellow |] :> Array
                ]
        )
       

    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 120.0 0.1 100.0 (float s.X / float s.Y))

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rotation = 
        win.Time |> AVal.map (fun _ -> 
            let t = sw.Elapsed.TotalSeconds
            Trafo3d.Rotation(V3d(1.0,0.1,0.0).Normalized, t) * Trafo3d.Rotation(V3d(0.1,1.0,0.0).Normalized, t * 0.5)
        )

    let sg =
        Sg.box' C4b.Red (Box3d(-V3d.III, V3d.III))
            |> Sg.scale 1.0
            |> Sg.translate -2.0 -2.0 -2.0
            |> Sg.trafo rotation
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                //DefaultSurfaces.simpleLighting |> toEffect
               ]
            |> Sg.diffuseTexture DefaultTextures.checkerboard

            //|> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

    let projs =
        let p = Frustum.perspective 90.0 0.1 100.0 1.0 |> Frustum.projTrafo
        [|
            p
            Trafo3d.RotationY(Constant.Pi) * p
            
            Trafo3d.RotationX(Constant.PiHalf) * Trafo3d.RotationZ(-Constant.PiHalf) * p
            Trafo3d.RotationX(-Constant.PiHalf) * Trafo3d.RotationZ(Constant.PiHalf) * p
            
            Trafo3d.RotationY(-Constant.PiHalf) * p
            Trafo3d.RotationY(Constant.PiHalf) * p
        |]

    let colors =
        [|
            C4f.Red
            C4f.Green
            C4f.Blue
            C4f.Yellow
            C4f.Cyan
            C4f.Magenta
        |]

    let tasks =
        projs |> Array.mapi (fun i p ->
            let task = 
                sg
            
                |> Sg.projTrafo' p
                |> Sg.compile app.Runtime win.FramebufferSignature

            RenderTask.ofList [
                app.Runtime.CompileClear(win.FramebufferSignature, colors.[i])
                task
            ]
        )
        |> CubeMap


    

    let res = RenderTask.renderToColorCubeMipWithUniformClear (AVal.constant 1024) (clear { color C4f.Green; depth 1.0}) tasks
    
    let sg =
        Sg.fullScreenQuad
        |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (AVal.map Frustum.projTrafo proj)
        |> Sg.texture "CubeMap" res
        |> Sg.shader {
            do! Shader.cubeMap
        }
    
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0
