new Model {
  val ns = Namespace("http://www.example.com/#")

  'foo --> (
    ns.bar > transform(att("id")) {
      string => string.drop(6)
      } {
        string => "myorg:" + string
    }
  )
}
