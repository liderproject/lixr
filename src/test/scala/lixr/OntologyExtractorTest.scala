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
        "http://purl.org/dc/terms/type"), Class(URI.create(
        "http://www.w3.org/ns/dcat#Dataset")), URI.create(
        "http://www.w3.org/2001/XMLSchema#string")))
    }
    "contain the class tbx:MartifHeader" in {
      tbxModel.classes should contain (Class(URI.create(
        "http://tbx2rdf.lider-project.eu/tbx#MartifHeader")))
    }
    "contains the property rdfs:label" in {
      tbxModel.dataProperties should contain (DataProperty(
        URI.create("http://www.w3.org/2000/01/rdf-schema#label"),
        Class(URI.create("http://www.w3.org/ns/lemon/ontolex#LexicalEntry")),
        URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")))
    }
  }
  "The MetaShare model" should {
    var metashareModel : Ontology = null
    "load" in {
      metashareModel = OntologyExtractor(models.Metashare, 
        "http://www.ilsp.gr/META-XMLSchema","resourceInfo").ontology
      val out = new java.io.PrintWriter("metashare.owl")
      out.println(metashareModel.toXML)
      out.flush
      out.close
    }
    "contain an individual named other of type SpeechInfluences" in {
      metashareModel.individuals should contain (Individual(
        URI.create("http://purl.org/ms-lod/MetaShare.ttl#other"),
        Class(URI.create("http://purl.org/ms-lod/MetaShare.ttl#SpeechInfluences"))))
    }
    "write csv" in {
      val descriptions = io.Source.fromFile("metashare-descriptions.tsv").getLines.map(_.split("\t")).toSeq.groupBy(_(0)).mapValues(_.toList.map(_.drop(1).mkString("\t")))
      writeClasses(metashareModel, descriptions)
      writeObjProperties(metashareModel, descriptions)
      writeDataProperties(metashareModel, descriptions)
      writeIndividual(metashareModel)
    }
    "write dot" in {
      writeDot(metashareModel, "MetaShare.dot", ms)
    }
  }

  val ms = "http://purl.org/ms-lod/MetaShare.ttl#"

  def dedupe[E <: Entity](left : Seq[E], right : E) = {
    if(left.isEmpty) {
      Seq(right)
    } else {
      if(left.last == right) {
        left
      } else {
        left :+ right
      }
    }
  }

  def writeClasses(ontology : Ontology, descriptions : Map[String, List[String]]) {
    val out = new java.io.PrintWriter("classes.csv")
    out.println("Identifier, Same As, Definitions, Superclass")
    val classes = ontology.classes.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe)
      
    for(clazz <- classes) {
      val uri = clazz.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",,\"" +
          descriptions.getOrElse(uri.drop(ms.length).take(1).toLowerCase +
            uri.drop(ms.length + 1), Nil).toSet.mkString("; ").replaceAll("\"","\\\\\"") +
          "\",")
      } else {
        out.println("," + uri + ",\"\",")
      }
    }
    out.flush
    out.close
  }

  def dropIfMs(uri : java.net.URI) = if(uri.toString.startsWith(ms)) {
    uri.toString.drop(ms.length)
  } else {
    uri.toString
  } 

  def writeObjProperties(ontology : Ontology, descriptions : Map[String, List[String]]) {
    val out = new java.io.PrintWriter("objprops.csv")
    out.println("Identifier, Same As, Definitions, Domain, Range")
    val props = ontology.objectProperties.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[ObjectProperty])

    for(prop <- props) {
      val uri = prop.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",,\"" +
          descriptions.getOrElse(uri.drop(ms.length), Nil).toSet.mkString("; ").replaceAll("\"", "\\\\\"") +
          "\"," + dropIfMs(prop.domain.name) + "," + 
          dropIfMs(prop.range.name))
      } else {
        out.println("," + uri +",\"\"," + dropIfMs(prop.domain.name) + "," + 
          dropIfMs(prop.range.name))
      }
    }
    out.flush
    out.close
  }
  def writeDataProperties(ontology : Ontology, descriptions : Map[String, List[String]]) {
      val out = new java.io.PrintWriter("dataprops.csv")
    out.println("Identifier, Same As, Definitions, Domain, Range")
    val props = ontology.dataProperties.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[DataProperty])

    for(prop <- props) {
      val uri = prop.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",,\"" +
          descriptions.getOrElse(uri.drop(ms.length), Nil).toSet.mkString("; ").replaceAll("\"", "\\\\\"") +
          "\"," + dropIfMs(prop.domain.name) + "," + 
          prop.range.toString.drop(prop.range.toString.indexOf("#") + 1))
      } else {
        out.println("," + uri +",\"\"," + dropIfMs(prop.domain.name) + "," + 
          prop.range.toString.drop(prop.range.toString.indexOf("#") + 1))
      }
    }
    out.flush
    out.close

  
  }
  def writeIndividual(ontology : Ontology) {
    val out = new java.io.PrintWriter("indivs.csv")
    out.println("Identifier, Same As, Type")
    val indivs = ontology.individuals.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[Individual])
    for(indiv <- indivs) {
      val uri = indiv.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",," + dropIfMs(indiv.`type`.name))
      } else {
        out.println("," + uri + "," + dropIfMs(indiv.`type`.name))
      }
    }
    out.flush
    out.close
  }

  def writeDot(ontology : Ontology, fileName : String, prefix : String) {
    val out = new java.io.PrintWriter(fileName)
    out.println("""digraph G {                                                          
  fontname = "Bitstream Vera Sans"                                              
  fontsize = 8                                                                  
                                                                                
  node [                                                                        
    fontname = "Bitstream Vera Sans"                                            
    fontsize = 8                                                                
    shape = "record"                                                            
  ]                                                                             
                                                                                
  edge [                                                                        
    fontname = "Bitstream Vera Sans"                                            
    fontsize = 8                                                                
  ]""")                                                                         
                                                                                
    def shorten(uri : String) = {                                                   
      uri.drop(math.max(uri.lastIndexOf("#"), uri.lastIndexOf("/")) + 1)            
    }                                                                               
                                                                                
    for(x <- ontology.classes) {
      val uri = x.name.toString
      if(uri.startsWith(prefix)) {
        val name = uri.drop(prefix.length)
        val label = new StringBuilder()
        label.append(name)
        for(y <- ontology.dataProperties.filter(_.asInstanceOf[Property[_]].domain == x)) {
          val range = y.asInstanceOf[DataProperty].range
          label.append("%s : %s\\l" format (shorten(y.name.toString), shorten(range.toString)))
        }
        if(label.toString != name) {
          label.insert(name.length, "|")
        }
        out.println("%s [ label=\"{%s}\" ]" format (name, label.toString))
      }
    }                                                                             
                                                                                  
    for(x <- ontology.objectProperties) {
      val elem = x.asInstanceOf[ObjectProperty]
      val uri = x.name.toString
      if(elem.domain.name.toString.startsWith(prefix) &&
         elem.range.name.toString.startsWith(prefix)) {
        out.println("%s -> %s [ label=\"%s\" ];" format (elem.domain.name.toString.drop(prefix.length),
                                                     elem.range.name.toString.drop(prefix.length),
                                                     shorten(uri)))
      }
    }
    out.println("}")
    out.flush()
    out.close()
  }
}

