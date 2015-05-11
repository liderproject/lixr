new Model {
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
}
