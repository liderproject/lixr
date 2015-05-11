new Model {
  val ns = Namespace("http://www.example.com/ns#")

  node("foo")(
    ns.p1 > "bar",
    ns.p2 > ("bar" @@ "en"),
    ns.p3 > ("bar" ^^ xsd.string),
    ns.p4 > ns.bar,
    ns.p5 > node("bar")(
      ns.p6 < ns.baz),
    ns.p7 < node("baz")()
  )
}
