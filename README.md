LIXR -- Lightweight Invertible XML to RDF Mapping
=================================================

LIXR (pronounced "Elixir") is a system designed to help the development of RDF models based on XML
documents. LIXR is based on XML stylesheet transforms but improves on this model
in a number of ways:

* Less Code
* Simple easy code
* Invertible, you can "round-trip" with a single file. 
* Can process large data files using streaming.
* Embedded into the Scala language for full meta-programming

Installation
------------

LIXR can be installed using [SBT](http://www.scala-sbt.org), the
binary can thus be compiled as follows

    sbt assembly

Running LIXR requires Java, and can be done with either the supplied script:

    ./lixr definition.scala input.xml

Or with Java:

    java -jar target/scala-2.11/lixr-assembly-0.1.jar definition.scala input.xml

Please not `definition.scala` is the LIXR conversion script written in the LIXR 
language and `input.xml` is the input XML file you wish to convert.

The LIXR Language
-----------------

LIXR is a domain-specific language implemented in Scala, as such all LIXR files
must be valid Scala files. At runtime LIXR definitions are evaluated and must
return an object of type `eu.liderproject.lixr.Model` and thus the most basic
definition of an LIXR mapping is as follows:

    new Model { }

The primary element of an LIXR model is a mapping, for example if we have an XML
tag `<example></example>` then we can map it to a comment (in the output RDF) as
follows

    new Model {
      'example --> comment("Hello, world!")
    }

Many XML documents use one or more namespaces, and thus it is normal that an
LIXR file will start with a `Namespace` declaration. Note that if a namespace is
declared even as the base namespace it should be used in the mapping. For
example, if we have the input file

    <example xmlns="http://www.example.com/"
             xmlns:foo="http://www.example.com/foo#">
      <foo:bar>baz</foo:bar>
    </example>

We can map as follows

    new Model {
      val base = Namespace("http://www.example.com/")
      val foo = Namespace("http://www.example.com/foo#")

      base.example --> comment("Example")

      foo.bar --> comment("Bar")
    }

Note that in LIXR a period is used instead of a colon to separate the namespace
from the entity name.

### Handlers and nodes

When generating RDF from XML, LIXR works by starting at the root tag and the
recursively following *handlers* which are declared as part of each mapping
element. A triple may also be generated by a statement separated by the `>`
symbol. So for example to generate a single triple from the previous XML we
would require a declaration such as

    new Model {
      val base = Namespace("http://www.example.com/")
      val foo = Namespace("http://www.example.com/foo#")
      
      base.example --> (
        handle(foo.bar)
      )

      foo.bar --> (
        foo.bar > "baz"
      )
    }

This will start at the `base.example` handler (as this is the root tag) and then
follows the handler for `foo.bar` and generates the following Turtle document

    @prefix : <http://www.example.com> .
    @prefix foo: <http://www.example.com/foo#> .

    <> foo.bar "baz" .

**Check!**

When generating the LIXR system always has a current *node*, which is where
triples will be generated this is generated with the `node()()` function. For
example we may adapt the previous handler as follows:

    base.example --> (
      node("http://www.example.com/node")(
        handle(foo.bar)
      )
    )

    foo.bar --> (
      foo.bar > "baz"
    )

In order to generate the following Turtle document:

    @prefix : <http://www.example.com> .
    @prefix foo: <http://www.example.com/foo#> .

    <http://www.example.com/node> foo.bar "baz" .

### Triple Generators

Triples may be generated in the following forms
* `ns.property > "text"`: Generates an untyped data property
* `ns.property > ("text" @@ "en")`: Generates a data property with a language tag
* `ns.property > ("text" ^^ "http://uri")`: Generates a data property with a
  datatype
* `ns.property > xmlContent(<foo></foo>)`: Generate an XML datatype literal,
  [see below](). ** Shouldn't this be plain **
* `ns.property > ns.name`: Generate a URI with a fixed value
* `ns.property > node("http://uri/")()`: Generates an object property whose object
  is the named node
* `ns.property > when()()`: Generates conditionally, see [below](#conditions)
  **check**!
* `ns.property < ns.name`: Generate a backlink, this means that the active node
  will be generated in the *object* position and `ns.name` will be the
  generated subject.
* `ns.property < node()()`: Generate a backlink and with the subject as anew active
  node

Example:

    node("foo")(
      ns.p1 > "bar",
      ns.p2 > ("bar" @@ "en"),
      ns.p3 > ("bar" ^^ xsd.string),
      ns.p4 > ns.bar,
      ns.p5 > node("bar")(
        ns.p6 < ns.baz),
      ns.p7 < node("baz")()
    )

Generates:

    <foo> ns:p1 "bar" ;
      ns:p2 "bar"@en ;
      ns:p3 "bar"^^xsd:string ;
      ns:p4 ns:bar ;
      ns:p5 <bar> .

    ns:baz ns:p6 <bar> .

    <baz> ns:p7 <foo> .

### Text Generators

Text generators are used to generate text based on the content of the current
document, the following generators are available

* `"fixed text"`: Generate a fixed text string
* `content`: The text contents of the current node and all its children
* `content('request)`: The content of the node selected by `'request`, see
  [requests](#requests).
* `xmlContent`, `xmlContent('request)`: The XML content (including tags and
  attributes) of the
  current node and all its children (or the nodes matching `'request`).
* `att("foo")`: The value of the XML attribute `foo`. A namespace identifier may
  als be used.
* `frag("foo")`: Generates a unique string of the form `#fooN` where `N` is a
  unique number counting from 1. This is intended for use as `node(frag("foo"))()`.
* `uuid`: Generate a UUID (a globally unique string).
* `get("foo")`: The value of the variable `foo`, see [variables](#variables)
* `"foo" +:` + _TextGenerator_, _TextGenerator_ `:+ "foo"`: Prepend or append a
  fixed string to the start of another text generator
* _TextGenerator_ `or` _TextGenerator_: Generate the second generator only if
  the first generator does not exist.
* `transform(){}{}`: An invertible transformation, the first argument of which
  should be a text generator, and the other two should be standard Scala
  functions with the signature `String => String`. The first represents the XML
  to RDF mapping function and the second the RDF to XML mapping function

An example of a transform to remove the text `myorg:` from an attribute `id` is
as follows

    transform(att("id")) {
      string => string.drop(6)
    } {
      string => "myorg:" + string
    }

Several built-in variations of `transform` exist, for example:

    def replace(tg : TextGenerator, regex1 : String, regex2 : String) = 
      transform(tg)(_.replaceAll(regex1, regex2))(_.replaceAll(regex2, regex1))

\emph{    def substring(tg : TextGenerator, start : Int, end : Int) =
      transform(tg)(_.slice(start, end))(throw new UnsupportedOperationException())}

### Requests

Requests function like XPath selectors to choose an element of the document
relevant to the current document, however these nodes are much simpler. The
selector syntax is as follows:

* _Request_ `\` _Request_: Find a direct child of the give name

Conditionals
------------

Conditionals may be used to model multiple options, and function like but should
not be confused with Scala's `if {} else if {} else {}` clause. The format of an
LIXR conditional is:

    when(condition)(
      generators
    ) or(condition)(
      generators
    ) otherwise(
      generators
    )

The following may be used as conditions

* _TextGenerator_ `===` _TextGenerator_: Checks equality of two text generators
* _TextGenerator_ `!==` _TextGenerator_: Checks inequality of two text
  generators
* _TextGenerator_ `.matches(regex)`: Checks if the text generator matches the
  current regular expression
* _TextGenerator_ `.exists`: Checks if the text generator fails to generate
  anything (primarily used to check if an attribute is present, the content of
  a tag always exists but may be the empty string)
* _Request_ `.exists`: Checks if a node(s) has children.
* `and`, `or`, `not`: Standard Boolean combinations of conditions.

### Variables

Variables may be set for convinience and to avoid long dependencies, the syntax
is as follows

    set("myvar", "foo")(
      comment(get("myvar"))
    )

Note the variable is only set for the generators that are called from the block
of generators set in the statement, for example:

    'foo --> (
      set("myvar", "foo")(
        comment(get("myvar")), // Succeeds!
        handle('bar)
      ),
      comment(get("myvar")) // Fails!
    )

    'bar --> (
      comment(get("myvar")) // Suceeds if called from 'foo
    )

### Iterators

Handlers are associated globally with a matching tag, however often a tag may
generate different RDF at different parts of a file. For this case, the `forall`
command should be used, e.g.,

    'foo --> (
      forall('name)(
        ns.nameOfFoo > content
      )
    )

    'bar --> (
      forall('name)(
        ns.nameOfBar > content
      )
    )

### Other generators

The following other generators are available 

* `comment()`: Generate a comment in the output file
* `message()`: Generate a message (to STDERR) during the generation
* `fail()`: Abort the conversion with the given message
* `xmlContent()`: In RDF2XML mode, include the given XML content at this point.
  In XML2RDF mode, generate a warning if the given XML content is not found.
* `xmlAtt("foo","bar")`: As above but verify only that the current node has an
  attribute with the given value.

LIXR as an embedded language
----------------------------

LIXR is embedded into Scala and each LIXR transform is first evaluated as a
Scala file. Some experience with Scala is helpful to understand error messages
and debugging files. We will now list some common pitfalls

### Strings are concatenated with `:+` and `+:` not `+`

The expression `content + "foo"` generates a text string such as
`ContentGenerator@abe890foo`. This is as Scala converts the `content` element to
a string and then concatenates the string.

### `if` and `else` are not used for conditions

Writing a condition using Scala primitives will cause it to be executed at
compile time, e.g.,

    if(content == "foo") {
      ns.prop > ns.value
    } else {
      ns.prop > ns.someOtherValue
    }

This will always generate `ns.value` and never `ns.someOtherValue` as the LIXR
generator object `content` is never equal to "foo". This should instead be
rewritten using LIXR constructs:

    when(content === "foo")(
      ns.prop > ns.value
    ) otherwise(
      ns.prop > ns.someOtherValue
    )

Note, `if` may be useful when checking global configuration variables, e.g.,

    if(System.getProperty("something") == "foo") {
      ns.prop > ns.value
    } else {
      ns.prop > ns.someOtherValue
    }
