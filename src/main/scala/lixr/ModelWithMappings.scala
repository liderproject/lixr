package eu.liderproject.lixr

trait ModelWithMappings extends Model {
  val owl = Namespace("http://www.w3.org/2002/07/owl#")
  val rdfs = Namespace("http://www.w3.org/2000/01/rdf-schema#")

  /** The name of the language property */
  protected def lang = "lang"

  def langStringMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      when(current.att(lang).exists)(
        rdf > (content @@ current.att(lang))
      ).otherwise(
        rdf > (content @@ "und")
      )
    )
    //node.when(current.att("lang").exists) --> (
    //  rdf > (content @@ current.att("lang"))
    //)

    //node --> (
    //  rdf > (content @@ "und")
    //)

    //handle(node)
  }

  def stringMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
         rdf > content
    )
   // node --> (
   //   rdf > content
   // )

   // handle(node)
  }

  def stringAttMap(node : Request, attName : String, rdf : NodeRequest) = {
    forall(node)(
      rdf > att(attName)
    )
  }

  def boolMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      rdf > (content ^^ xsd.boolean)
    )
    //node --> (
    //  rdf > (content ^^ xsd.boolean)
    //)
    //handle(node)
  }

  def intMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      rdf > (content ^^ xsd.int)
    )
    //node --> (
    //  rdf > (content ^^ xsd.int)
    //)
    //handle(node)
  }

  def doubleMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      rdf > (content ^^ xsd.decimal)
    )
    //node --> (
    //  rdf > (content ^^ xsd.decimal)
    //)
    //handle(node)
  }


  def objectMap(node : NodeRequest, rdf : NodeRequest, values : (String,NodeRequest)*) = {
    assert(values.size > 1)
    forall(node) {
      var gen : ConditionalGenerator = when(content === values.head._1)(
        rdf > values.head._2
      )
      for((k,v) <- values.tail) {
        gen = gen.or(content === k)(rdf > v)
      }
      gen
    }

    //for((k,v) <- values) {
    //  node.when(content === k) --> (
    //    rdf > v
    //  )
    //}
    //handle(node)
  }

  def dataMap(node : NodeRequest, rdf : NodeRequest, values : (String,String)*) = {
    assert(values.size > 1)
    forall(node) {
      var gen : ConditionalGenerator = when(content === values.head._1)(
        rdf > text(values.head._2)
      )
      for((k, v) <- values.tail) {
        gen = gen.or(content === k)(rdf > text(v))
      }
      gen
    }
    //for((k,v) <- values) {
    //  node.when(content === k) --> (
    //    rdf > (text(v) @@ "eng")
    //  )
    //}
    //handle(node)
  }

  def dateMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      rdf > (content ^^ xsd.date)
    )
    //node --> (
    //  rdf > (content ^^ xsd.date)
    //)
    //handle(node)
  }

  def linkMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      rdf > uri(content(rdf))
    )
    //node --> (
    //  rdf > uri(content(rdf))
    //)
    //handle(node)
  }

  def yearMap(node : NodeRequest, rdf : NodeRequest) = {
    forall(node)(
      when(content matches "\\d{3,4}")(
        (rdf > (content ^^ xsd.gYear)) : Generator
      ).otherwise(
        fail(text("Badly formatted year "), content)
      )
    )
      
    //node.when(content matches "\\d{3,4}") --> (
    //  rdf > (content ^^ xsd.gYear)
    //)
    //node --> (
    //  fail(text("Badly formatted year "), content)
    //)
    //handle(node)
  }

  def typedMap(node : NodeRequest, rdf : NodeRequest, datatype : NodeRequest) = {
    forall(node)(
      rdf > (content ^^ datatype)
    )
  }
}
