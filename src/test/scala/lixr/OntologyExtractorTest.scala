package eu.liderproject.lixr

import org.scalatest._
import java.net.URI

class OntologyExtractorTest extends WordSpec with Matchers {
  var tbxModel : Ontology = null
  "The TBX model" should {
    "load" in {
      val tbx = new models.TBX("", new java.io.File("src/test/resources/mappings.blank"))
      tbxModel = OntologyExtractor(tbx, 'martif).ontology
      /*for(e <- tbxModel.entities) {
        println(e) 
      }*/
    }
    "contain the class dcat:Dataset" in {
      tbxModel.classes should contain (Class(URI.create(
        "http://www.w3.org/ns/dcat#Dataset")))
    }
    "contain the property dct:type" in {
      tbxModel.dataProperties should contain (DataProperty(URI.create(
        "http://purl.org/dc/terms/type"), Seq(Class(URI.create(
        "http://www.w3.org/ns/dcat#Dataset"))), URI.create(
        "http://www.w3.org/2001/XMLSchema#string")))
    }
    "contain the class tbx:MartifHeader" in {
      tbxModel.classes should contain (Class(URI.create(
        "http://tbx2rdf.lider-project.eu/tbx#MartifHeader")))
    }
    "contains the property rdfs:label" in {
      tbxModel.dataProperties should contain (DataProperty(
        URI.create("http://www.w3.org/2000/01/rdf-schema#label"),
        Seq(Class(URI.create("http://www.w3.org/ns/lemon/ontolex#LexicalEntry"))),
        URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")))
    }
  }
  "The MetaShare model" should {
    var metashareModel : Ontology = null
    "load" in {
      metashareModel = OntologyExtractor(models.Metashare, 
        "http://www.ilsp.gr/META-XMLSchema","resourceInfo").ontology
    }
    "contain an individual named other of type SpeechInfluences" in {
      metashareModel.individuals should contain (Individual(
        URI.create("http://purl.org/net/def/metashare#other"),
        Class(URI.create("http://purl.org/net/def/metashare#SpeechInfluences"))))
    }
//    "write csv" in {
//      val descriptions = io.Source.fromFile("metashare-descriptions.tsv").getLines.map(_.split("\t")).toSeq.groupBy(_(0)).mapValues(_.toList.map(_.drop(1).mkString("\t")))
//      writeClasses(metashareModel, descriptions)
//      writeObjProperties(metashareModel, descriptions)
//      writeDataProperties(metashareModel, descriptions)
//      writeIndividual(metashareModel)
//    }
//    "write dot" in {
//      writeDot(metashareModel, "MetaShare.dot", ms)
//    }
  }

  val ms = "http://purl.org/net/def/metashare#"


}

