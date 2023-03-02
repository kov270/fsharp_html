module Main

open Prog

[<EntryPoint>]
let main argv =
    let input = System.Console.In.ReadToEnd()
    parseSmf input |> buildTreeAndPrint 4 |> ignore
    // printfn "%A" (parseSmf input |> buildTreeAndPrint 4)
    // printfn "%A" (parseHtml argv[0])
    0
