module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Prog
open System.IO
open System

let originalOut = Console.Out

let indexMissingClosingTag = """<!doctype html>
<html>
<head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>

</body>
</html>
"""
let indexTooMuchClosingTag = """
<!doctype html>
<html>
<head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />   
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
</div>
"""

[<Fact>]
let ``too_much_closing_tag``() =
    try
        indexTooMuchClosingTag |> parseSmf |> buildTreeAndPrint 4 |> ignore
        Assert.Fail("no ex")
    with
    | ex -> Assert.Contains("unexpected closing tag",ex.Message)


[<Fact>]
let ``missing_closing_tag``() =
    try
        indexMissingClosingTag |> parseSmf |> buildTreeAndPrint 4 |> ignore
        Assert.Fail("no ex")
    with
    | ex -> Assert.Contains("wrong clousing tags", ex.Message)


let goodOutput = """<!DOCTYPE html>
<html>
    <head>
        <title>
Example Domain
        </title>
        <meta charset=utf-8>
        <meta http-equiv=Content-type content=text/html; charset=utf-8>
        <meta name=viewport content=width=device-width, initial-scale=1>
        <style type=text/css>
body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        
    }
    div {
        width: 600px;
        margin: 5em auto;
        padding: 2em;
        background-color: #fdfdff;
        border-radius: 0.5em;
        box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);
    }
    a:link, a:visited {
        color: #38488f;
        text-decoration: none;
    }
    @media (max-width: 700px) {
        div {
            margin: 0 auto;
            width: auto;
        }
    }
    
        </style>
    </head>
    <body>
        <div>
            <h1>
Example Domain
            </h1>
            <p>
This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.
            </p>
            <p>
                <a href=https://www.iana.org/domains/example>
More information...
                </a>
            </p>
        </div>
    </body>
</html>
"""

let goodButShortInput = """<!doctype html>
<html>
<head><title>Example Domain</title>
<meta charset="utf-8" /><meta http-equiv="Content-type" content="text/html; charset=utf-8" /><meta name="viewport" content="width=device-width, initial-scale=1" /><style type="text/css">
    body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        
    }
    div {
        width: 600px;
        margin: 5em auto;
        padding: 2em;
        background-color: #fdfdff;
        border-radius: 0.5em;
        box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);
    }
    a:link, a:visited {
        color: #38488f;
        text-decoration: none;
    }
    @media (max-width: 700px) {
        div {
            margin: 0 auto;
            width: auto;
        }
    }
    </style>    
</head><body>
<div><h1>Example Domain</h1><p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p><p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div></body></html>
"""


// Fail because of parallel execution
// [<Fact>]
// let ``normal_work``() =
//     use writer = new System.IO.StringWriter()
//     Console.SetOut writer
//     good_but_short_input |> parseSmf |> buildTreeAndPrint 4 |> ignore
//     let actualOutput = 
//         writer.ToString()
    
//     // Console.SetOut(originalOut)
//     Console.SetOut(Console.Out)
//     writer.Close() 
//     Assert.Equal(good_output, actualOutput)
