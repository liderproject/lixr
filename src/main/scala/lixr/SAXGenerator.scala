package eu.liderproject.lixr

import javax.xml.parsers.{SAXParserFactory}
import java.io.InputStream
import org.xml.sax.{Attributes,ContentHandler,InputSource,Locator}

class SAXGenerator(model : Model) extends ContentHandler {
  override def characters(ch : Array[Char], start : Int, length : Int) { }
  override def endDocument() { }
  override def endElement(uri : String, localName : String, qName : String) { }
  override def endPrefixMapping(prefix : String) { }
  override def ignorableWhitespace(ch : Array[Char], start : Int, length : Int) { }
  override def processingInstruction(target : String, data : String) { }
  override def setDocumentLocator(locator : Locator) { }
  override def skippedEntity(name : String) { }
  override def startDocument() { }
  override def startElement(uri : String, localName : String, qName : String, atts : Attributes) { }
  override def startPrefixMapping(prefix : String, uri : String) { }


  def read(file : InputStream) {
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()
    val reader = parser.getXMLReader()
    reader.setContentHandler(this)
    reader.parse(new InputSource(file))
  }
}
