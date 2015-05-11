package eu.liderproject.lixr

import org.scalatest._


object testmodels {
  lazy val Metashare = Main.loadModelFromFile("models/metashare.scala")
  System.setProperty("tbx.resourceURI", "")
  System.setProperty("tbx.mappings", "src/test/resources/mappings.blank")
  lazy val tbx = Main.loadModelFromFile("models/tbx.scala")
  lazy val ex1 = Main.loadModelFromFile("src/test/resources/readme/ex1.scala")
  lazy val ex2 = Main.loadModelFromFile("src/test/resources/readme/ex2.scala")
//  lazy val ex3 = Main.loadModelFromFile("src/test/resources/readme/ex3.scala")
  lazy val ex4 = Main.loadModelFromFile("src/test/resources/readme/ex4.scala")
  lazy val ex5 = Main.loadModelFromFile("src/test/resources/readme/ex5.scala")
  lazy val ex6 = Main.loadModelFromFile("src/test/resources/readme/ex6.scala")
  lazy val ex7 = Main.loadModelFromFile("src/test/resources/readme/ex7.scala")
  lazy val ex8 = Main.loadModelFromFile("src/test/resources/readme/ex8.scala")
  lazy val ex9 = Main.loadModelFromFile("src/test/resources/readme/ex9.scala")
  lazy val ex10 = Main.loadModelFromFile("src/test/resources/readme/ex10.scala")
}

class MetashareTest extends FlatSpec with Matchers { 
  import testmodels._

  "trivial document" should "produce nothing" in {
    val doc = "<OAI-PMH xmlns='http://www.openarchives.org/OAI/2.0/'/>"
    val in = new java.io.StringReader(doc)
    val gen = new DOMGenerator()
    val result = gen.read(in, Metashare)
    result should be (Seq())
  }

  "simple document" should "produce something" in {
    val doc = """<OAI-PMH xmlns='http://www.openarchives.org/OAI/2.0/'>
    <responseDate>2014-10-03T11:18:32Z</responseDate>
    <request verb="ListRecords" metadataPrefix="metashare">http://127.0.0.1:8000/oai_pmh/</request>
    <ListRecords>
      <record>
        <header/>
      </record>
    </ListRecords>
</OAI-PMH>"""
    val in = new java.io.StringReader(doc)
    val gen = new DOMGenerator()
    val result = gen.read(in, Metashare)
    result should not be (Seq())
  }

  "complex document" should "produce lots of things" in {
    val in = new java.io.FileReader("src/test/resources/metashare_example.xml")
    val gen = new DOMGenerator()
    val result = gen.read(in, Metashare)
  }

  "namespace" should "count names" in {
    //print(Metashare.msxml.getAllNames.size)
  }
}
