package eu.liderproject.lixr.models

import eu.liderproject.lixr._

object GrAF(base : Namespace) extends Model {
  val graf = Namespace("http://www.xces.org/ns/GrAF/1.0/")
  val nif = Namespace("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#")

  graf.fs --> (
    handle(graf.f),
    handle(att("type")),
    handle(att("feat")),
    handle(att("id")),
    handle(att("n")),
    handle(att("rend")),
    handle(att("rendition"))
  )

  graf.f.when(att("name") === "base") --> (
    nif.anchorOf > att("value")
  )
  // other graf.f
  
  graf.f.when(att("value").exists) --> (
    uri(base, att("name")) > att("value")
  )

  graf.f.when(att("fVal").exists) --> (
    uri(base, att("name")) > uri(att("fVal"))
  )
}






