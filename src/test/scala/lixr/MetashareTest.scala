package eu.liderproject.lixr

import eu.liderproject.lixr.models.Metashare
import org.scalatest._



class MetashareTest extends FlatSpec with Matchers { 
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
