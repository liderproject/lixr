package eu.liderproject.lixr

import org.scalatest._
import scala.xml._

class DOMGeneratorTest extends FlatSpec with Matchers {
  import testmodels._
  val doc = """<OAI-PMH xmlns='http://www.openarchives.org/OAI/2.0/'>
    <responseDate>2014-10-03T11:18:32Z</responseDate>
    <request verb="ListRecords" metadataPrefix="metashare">http://127.0.0.1:8000/oai_pmh/</request>
    <ListRecords>
      <record>
        <header/>
      </record>
    </ListRecords>
</OAI-PMH>"""


  "getHandlers" should "find some results" in {
    val result = Metashare.getHandlers((Some("http://www.openarchives.org/OAI/2.0/"),"OAI-PMH"))
    result should not be (Seq())
  }

  "locate" should "find something" in {
    val xml = XML.loadString(doc)
    val gen = new DOMGenerator()
    val result = gen.locate(Metashare.current, gen.State(xml, Metashare, None, Map()))
    result should not be (Seq())
  }

}
