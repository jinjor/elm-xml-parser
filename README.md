XmlParser
====

[![Build Status](https://travis-ci.org/jinjor/elm-xml-parser.svg)](https://travis-ci.org/jinjor/elm-xml-parser)

XML Parser for Elm

## How to use

There is only two functions available.

```elm
parse : String -> Result Parser.Error Xml
format : Xml -> String
```

Typically, you'll use `parse` function, get the root node and traverse it.

```elm
> import XmlParser
> XmlParser.parse """<a name="value">foo</a>"""
Ok { processingInstructions = [], docType = Nothing, root = Element "a" ([{ name = "name", value = "value" }]) ([Text "foo"]) }
```

I'm not going to make decoder and encoder right now. Please let me know if you are interested :)

## LICENSE

BSD-3-Clause
