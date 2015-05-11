new Model {
  'foo --> (
    transform(att("id")) {
      string => string.drop(6)
      } {
        string => "myorg:" + string
    }
  )
}
