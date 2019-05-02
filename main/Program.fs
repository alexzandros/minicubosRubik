open System
open Cubos

[<EntryPoint>]
let main argv =
    initialCube
    |> turnFace (Left 4)
    |> rotateCube XQuarterBack
    |> turnFace (Left 4)
    |> rotateCube YQuarterLeft
    |> rotateCube XQuarterFront
    |> turnFace (Left 4)
    |> rotateCube XQuarterFront
    |> turnFace (Left 4)
    |> cubeToString
    |> printfn "%s"
    0 // return an integer exit code
