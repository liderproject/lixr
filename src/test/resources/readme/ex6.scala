new Model {
  val base = Namespace("http://www.example.com/#")
  val foo = Namespace("http://www.example.com/bar#")

  base.example --> (
    node("http://www.example.com/node")(
      handle(foo.bar)
    )
  )
  
  foo.bar --> (
    foo.bar > "baz"
  )
}
