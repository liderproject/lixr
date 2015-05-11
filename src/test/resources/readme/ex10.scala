new Model {
  val ns = Namespace("http://www.example.com/#")

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
}
