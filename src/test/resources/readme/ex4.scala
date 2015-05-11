new Model {
  val base = Namespace("http://www.example.com/")
  val foo = Namespace("http://www.example.com/foo#")

  base.example --> comment("Example")

  foo.bar --> comment("Bar")
}
