## Лабораторная №4

<b>Выполнил:</b> Коваленко Илья Русланович \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### HTML parser
Делаем тип HtmlTag, который основной тип, благодаря IsClosing IsClosed, их последовательность можно собрать в дерево. 
```f#
type HtmlTag =
    {
        Name: string
        Attributes: (string * string) list
        IsClosing: bool
        IsClosed: bool
    }
```

documentParser - основной парсер собранный через других мелких, tagParser - парсит обычный тэг
```f#
// Define a parser for parsing HTML documents
let documentParser = many ((doctypeParser.>>whitespaceParser) <|> (tagParser.>>whitespaceParser) <|> ((many1Satisfy (fun c -> c <> '<') |>> fun text -> { Name = "text"; Attributes = [("text", text)]; IsClosing = false; IsClosed = true }).>>whitespaceParser))

let tagParser =
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

```


### Работа
```
> cat index.html | dotnet run 
<!DOCTYPE html>
<html>
    <head>
        <title>
Example Domain
        </title>
        <meta charset=utf-8>
        <meta http-equiv=Content-type content=text/html; charset=utf-8>
        <meta name=viewport content=width=device-width, initial-scale=1>
        <style type=text/css>
...
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

```

### Выводы
Стандарт HTML очень свободный, годы эволюции браузеров привели к большой свободе написания html, который довольно сложно привести к полностью читаемому формату (например отсутствующие закрытия тэгов, коротые закрываються разными браузерами по своему).
