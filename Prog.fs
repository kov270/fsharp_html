module Prog

open FParsec
open System

// Define a type to represent an HTML tag
type HtmlTag =
    {
        Name: string
        Attributes: (string * string) list
        IsClosing: bool
        IsClosed: bool
    }

let whitespaceParser = manySatisfy Char.IsWhiteSpace

let doctypeParser = 
    // printf "!doctypeParser"
    let doctypePrefixParser = pstringCI "<!DOCTYPE"
    between (doctypePrefixParser >>. whitespaceParser) (pchar '>') 
        (many1Satisfy (fun c -> c <> '>') |>>
            (fun body -> 
                 { Name = "!DOCTYPE"; Attributes = [(body,"")]; IsClosing = false; IsClosed = true}))


// Define a parser for parsing HTML tags
let tagParser =
    // let nameParser = (pstringCI "img" <|> pstringCI "br" <|> pstringCI "hr" <|> pstringCI "input" <|> many1Satisfy isLetter) .>> whitespaceParser
    let nameParser = 
        manySatisfy (fun c -> Char.IsLetterOrDigit c || c = '-') .>> whitespaceParser
    let closingseqParser = (pstring "/>" <|> pstring ">")
    let attributeParser = 
        let attributeNameParser = 
            // many1Satisfy isLetter
            // many1Satisfy (fun c -> c <> '=' && not (Char.IsWhiteSpace c))
            manySatisfy (fun c -> c <> '=' && c <> '>' && c <> '/' && not (Char.IsWhiteSpace c))

        let attributeValueParser = 
            let quoteParser = pchar '\"' <|> pchar '\''
            between quoteParser (quoteParser) (manySatisfy (fun c -> c <> '\"' && c <> '\''))
        pipe2 attributeNameParser (pchar '=' >>. attributeValueParser) (fun name value -> (name, value))
    // WFT whitespaceParser outside many ignored
    let attributeListParser = (many (spaces >>. attributeParser.>> whitespaceParser))
    // TODO refactor, any ideas?
    let openingTagParser = 
        (pchar '<' >>. whitespaceParser) >>. (pipe2 nameParser attributeListParser (fun name attributes -> { Name = name; Attributes = attributes; IsClosing = false; IsClosed = true })) .>>. closingseqParser 
            |>> fun value -> 
                match value with
                | (tag, "/>") -> { Name = tag.Name; Attributes = tag.Attributes; IsClosing = false; IsClosed = true }
                | (tag, _) -> { Name = tag.Name; Attributes = tag.Attributes; IsClosing = false; IsClosed = false }

        // (between (pchar '<' >>. whitespaceParser) (pstring ">") (pipe2 nameParser attributeListParser (fun name attributes -> { Name = name; Attributes = attributes; IsClosing = false; IsClosed = false })))
    let closingTagParser = 
        between (pstring "</" >>. whitespaceParser) (closingseqParser) (nameParser |>> fun name -> { Name = name; Attributes = []; IsClosing = true; IsClosed = true })
    choice [closingTagParser; openingTagParser]

// Define a parser for parsing HTML documents
let documentParser = many ((doctypeParser.>>whitespaceParser) <|> (tagParser.>>whitespaceParser) <|> ((many1Satisfy (fun c -> c <> '<') |>> fun text -> { Name = "text"; Attributes = [("text", text)]; IsClosing = false; IsClosed = true }).>>whitespaceParser))
// let documentParser = many (tagParser <|> (many1 digit |>> fun text -> { Name = "?"; Attributes = [("text", new System.String (Array.ofList(text) ))]; IsClosing = false }))

// Define a function for parsing HTML documents
let parseHtml (input: string) =
    let result = run documentParser input
    match result with
    | Success(value, _, _) -> value
    | Failure(errorMsg, _, _) -> failwith errorMsg

let parseSmf (input: string) =
    let result = run documentParser input
    // printfn "Unparsed: %s" (Reply result)
    match result with
    | Success(value, a, b) -> 
        // printfn "Unparsed: %A" b
        // printfn "?: %A" a
        // printfn "P: %A" value
        value
    | Failure(errorMsg, a, b) -> failwith errorMsg

let printAtribute (a: list<string * string>) =
    List.fold (fun s a -> 
    s+" "+ 
        match a with
        | (f, "") -> f
        | (f, s) -> f+"="+s
    ) "" a

let buildTreeAndPrint (indent: int) (input: list<HtmlTag>) =
    List.fold (fun (stack: string list) tag ->
        // printfn "====%A" stack
        let ind = indent * stack.Length
        match tag.IsClosing, stack with
        | true, x::xs -> 
            if stack.Length = 0 then
                failwith "unexpected closing tag"
            
            printfn "%s</%s>" (String.replicate (ind-indent) " ") (tag.Name)
            xs
        | _, stack ->
            if tag.Name = "text" then
                printfn "%s" (snd tag.Attributes[0])
            else
                printfn "%s<%s%s>" (String.replicate ind " ") (tag.Name) (printAtribute tag.Attributes)
            if tag.IsClosed then
                stack
            else
                tag.Name :: stack
    ) [] input


let buildTreeAndPrintOld (indent: int) (input: list<HtmlTag>) =
    List.fold (fun (stack: string list) tag ->
        // printfn "====%A" stack
        let ind = indent * stack.Length
        printfn "%s<%s>" (String.replicate ind " ") (tag.Name)
        match tag.IsClosing, stack with
        | true, x::xs -> 
            if stack.Length = 0 then
                failwith "unexpected closing tag"
            xs
        | _, stack ->
            if tag.IsClosed then
                stack
            else
                tag.Name :: stack
    ) [] input


