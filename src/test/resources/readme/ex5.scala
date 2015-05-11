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
