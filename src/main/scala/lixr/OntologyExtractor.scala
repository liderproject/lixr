package eu.liderproject.lixr

import java.net.URI
import org.apache.commons.lang3.StringEscapeUtils._

trait Entity {
  def name : URI
  def toXML : String
  var definition : Option[String] = None
  protected def defXML = definition match {
    case Some(d) =>
      "    <rdfs:comment xml:lang=\"en\">%s</rdfs:comment>\n" format escapeXml10(d)
    case None =>
      ""
  }
      
}

case class Class(name : URI) extends Entity {
  def toXML = "  <owl:Class rdf:about=\"%s\">%s</owl:Class>" format (name, defXML)
}

object TopClass extends Class(URI.create("")) {
  override def toXML = ""
}

trait Property[A] extends Entity {
  def domain : Seq[Class]
  def range : A

  protected def propDomRangStr(dr : Seq[Class], name : String) = dr match {
    case Seq() => ""
    case Seq(TopClass) => ""
    case Seq(c) => 
      "    <rdfs:%s rdf:resource=\"%s\"/>\n" format(name, c.name)
    case dr => 
      ("   <rdfs:%s>\n" +
       "     <owl:Class>\n" +
       "       <owl:unionOf rdf:parseType=\"Collection\">\n" + (dr.map { c =>
       "         <rdf:Description rdf:about=\"%s\"/>\n" format(c.name)
       }).mkString("") +
       "       </owl:unionOf>\n" +
       "     </owl:Class>\n" +
       "   </rdfs:%s>\n") format(name, name)
  }
}

case class ObjectProperty(name : URI, domain : Seq[Class], range : Seq[Class]) extends Property[Seq[Class]] {
  def join(op : ObjectProperty) = {
    val op2 = ObjectProperty(name, domain ++ op.domain, 
      range ++ op.range)
    op2.definition = definition
    op2
  }

  def toXML = """  <owl:ObjectProperty rdf:about="%s">
%s%s%s</owl:ObjectProperty>""" format (name, propDomRangStr(domain, "domain"), propDomRangStr(range, "range"), defXML)
}

case class DataProperty(name : URI, domain : Seq[Class], range : URI) extends Property[URI] {
  def join(op : DataProperty) = {
    val op2 = DataProperty(name, domain ++ op.domain, range)
    op2.definition = definition
    op2
  }

  def toXML = """  <owl:DatatypeProperty rdf:about="%s">
%s    <rdfs:range rdf:resource="%s"/>
%s  </owl:DatatypeProperty>""" format (name, propDomRangStr(domain, "domain"), range, defXML)
}

case class Individual(name : URI, `type` : Class) extends Entity {
  private val qname = "(.*?)(\\w+)$".r
  def toXML = {
    val qname(pre, suf) = `type`.name.toString
    """  <ns:%s rdf:about="%s" xmlns:ns="%s">%s</ns:%s>""" format (suf, name, pre, defXML, suf)
  }
}
    

case class Ontology(entities : Set[Entity]) {
  def classes = entities.filter(_.isInstanceOf[Class])
  def properties = entities.filter(_.isInstanceOf[Property[_]])
  def objectProperties = entities.filter(_.isInstanceOf[ObjectProperty]).
    map(_.asInstanceOf[ObjectProperty]).
    groupBy(_.name).map(_._2.reduce((x,y) => x.join(y)))
  def dataProperties = entities.filter(_.isInstanceOf[DataProperty]).
    map(_.asInstanceOf[DataProperty]).
    groupBy(_.name).map(_._2.reduce((x,y) => x.join(y)))
  def individuals = entities.filter(_.isInstanceOf[Individual])

  def toXML = """<rdf:RDF
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  
  <rdfs:Datatype rdf:about="http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"/>
  
""" + (classes ++ objectProperties ++ dataProperties ++ individuals).map(_.toXML).mkString("\n\n") + """</rdf:RDF>"""

  def toXML(prefix : String) = """<rdf:RDF
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  
  <rdfs:Datatype rdf:about="http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"/>
  
""" + (classes ++ objectProperties ++ dataProperties ++ individuals.filter(_.asInstanceOf[Individual].`type`.name.toString.startsWith(prefix))).
  filter(_.name.toString.startsWith(prefix)).map(_.toXML).mkString("\n\n") + """</rdf:RDF>"""


}

class OntologyExtractor(root : Seq[(Model#Request, Seq[Model#Generator])], model : Model) {
  private val xsd = "http://www.w3.org/2001/XMLSchema#"
  private val rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  private val rdfs = "http://www.w3.org/2000/01/rdf-schema#"

  import model._
  def ontology : Ontology = {
    Ontology((root.flatMap { 
      case (_, gens) => 
        handle(gens)._2
    }).toSet)
  }

  def handle(gens : Seq[Model#Generator]) : (Option[Class], Seq[Entity]) = {
    val clazz = gens.flatMap(handleClass(_)).headOption
    (clazz, gens.flatMap(handle(clazz, _)))
  }

  def handleClass(gen : Model#Generator) 
      : Option[Class] = gen match {
    case OTripleGenerator(prop, value) if prop == rdf_type =>
      Some(Class(nr2uri(value)))
    case RecursiveGenerator(request) => 
      handleClassRequest(request)
    case GeneratorList(first, second) =>
      handleClass(first) match {
        case Some(c) => 
          Some(c)
        case None =>
          handleClass(second)
      }
    case _ =>
      None
  }

  private val hcrCache = collection.mutable.Map[Model#Request, Option[Class]]()
  def handleClassRequest(request : Model#Request) 
      : Option[Class] = hcrCache.get(request) match {
        case Some(c) =>
          c
        case None => request match {
          case NodeRequest(ns, FixedTextGenerator(n)) =>
            hcrCache.put(request, None)
            val e = (getHandlers(ns, n).flatMap {
              case (r, gens) =>
                gens.flatMap(handleClass(_))
            }).headOption
            hcrCache.put(request, e)
            e
          case _ =>
            None
      }
  }


  private val hnrCache = collection.mutable.Map[(Option[Class], Model#Request), Seq[Entity]]()
  def handleNodeRequest(clazz : Option[Class], request : Model#Request) 
      : Seq[Entity] = hnrCache.get((clazz, request)) match {
    case Some(e) =>
      e
    case None => request match {
      case NodeRequest(ns, FixedTextGenerator(n)) =>
        hnrCache.put((clazz, request), Nil)
        val e = getHandlers(ns, n).flatMap {
          case (r, gens) =>
            gens.flatMap(handle(clazz, _))
        }
        hnrCache.put((clazz, request), e)
        e
      case _ =>
        Nil
    }
  }

  def handle(clazz : Option[Class], gen : Model#Generator) 
      : Seq[Entity] = gen match {
    case NodeGenerator(_, body) =>
      handle(body)._2
    case RecursiveGenerator(request) => 
      handleNodeRequest(clazz, request) 
    case DTripleGenerator(prop, value) => 
      Seq(
        DataProperty(
          nr2uri(prop),
          clazz.toSeq,
          tr2class(value)
        )
      )
    case OTripleGenerator(prop, value) if prop == rdf_type =>
      Seq(Class(nr2uri(value)))
    case OTripleGenerator(prop, value) 
      if prop == (Namespace("http://www.w3.org/2002/07/owl#") + "sameAs") =>
    Seq(
      Individual(
        nr2uri(value),
        clazz.getOrElse(TopClass))
    )
    case OTripleGenerator(prop, value) =>
     val c = Class(
      capnr2uri(prop)
     )
     //if(value.name == FixedTextGenerator("other")) {
     //  println(prop)
     //  println(value)
     //  println(c)}
     Seq(
       ObjectProperty(
         nr2uri(prop),
         clazz.toSeq,
         Seq(c)),
       Individual(
         nr2uri(value),
         c),
       c)
    case NTripleGenerator(prop, NodeGenerator(_, body)) =>
      val (rangeClass, velems) = handle(body)
      ObjectProperty(
        nr2uri(prop),
        clazz.toSeq,
        rangeClass.toSeq
      ) +: velems
    case INTripleGenerator(prop, NodeGenerator(_, body)) =>
      val (domainClass, velems) = handle(body)
      ObjectProperty(
        nr2uri(prop),
        domainClass.toSeq,
        clazz.toSeq
      ) +: velems
    case ConditionalGenerator(_, r, o) => 
      o match {
        case Some(o) =>
          r.flatMap(handle(clazz, _)) ++ handle(clazz, o)
        case None =>
          r.flatMap(handle(clazz, _))
      }
    case GeneratorList(f, s) =>
      handle(clazz, f) ++ handle(clazz, s)
    case SetVariable(_, _, context) =>
      context.flatMap(handle(clazz, _))
    case ForGenerator(req, body) =>
      body.flatMap(handle(clazz, _))
    case c : CommentGenerator =>
      Nil
    case f : FailGenerator =>
      Nil
    case a : AttributeGenerator =>
      Nil
    case _ =>
      println(gen)
      Nil
  }

  def gen2uri(gen : Model#Generator) : URI = gen match {
    case OTripleGenerator(_, value) =>
      nr2uri(value)
    case _ =>
      throw new RuntimeException("Expected OTripleGenerator")
  }

  def nr2uri(nr : Model#NodeRequest) : URI = nr match {
    case NodeRequest(Some(ns), name) => 
      URI.create(ns + genString(name))
    case NodeRequest(None, name) =>
      URI.create(genString(name))
  }

  def capnr2uri(nr : Model#NodeRequest) : URI = nr match {
    case NodeRequest(Some(ns), name) => 
      val s = genString(name)
      URI.create(ns + s.take(1).toUpperCase + s.drop(1))
    case NodeRequest(None, name) =>
      URI.create(genString(name))
  }



  def genString(t : Model#TextGenerator) : String = t match {
    case FixedTextGenerator(s) => s
    case URIGenerator(s) =>
      genString(s)
    case _ => 
      throw new NotSchematic("Expected FixedTextGenerator got " + t)
  }

  def tr2class(t : TextGenerator) : URI = t match {
    case f : PlainTextGenerator => 
      URI.create(xsd + "string")
    case TypedTextGenerator(_, t) =>
      try {
        nr2uri(t) 
      } catch {
        case x : NotSchematic =>
          URI.create("")
      }
    case _ : LangTextGenerator =>
      URI.create(rdf + "langString")
    case _ : XMLTextGenerator =>
      URI.create(rdf + "XMLLiteral")
  }
}

object OntologyExtractor {
  def apply(model : Model, namespace : String, name : String) = {
    new OntologyExtractor(model.getHandlers((Some(namespace), name)), model)
  }
  def apply(model : Model, symbol : Symbol) = {
    new OntologyExtractor(model.getHandlers((None, symbol.toString.drop(1))), model)
  }

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

  def writeClasses(ontology : Ontology, descriptions : Map[String, List[String]], ms : String, outFile : String) {
    val out = new java.io.PrintWriter(outFile)
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

  def dropIfMs(uri : java.net.URI, ms : String) = if(uri.toString.startsWith(ms)) {
    uri.toString.drop(ms.length)
  } else {
    uri.toString
  } 

  def writeObjProperties(ontology : Ontology, descriptions : Map[String, List[String]], ms : String, outFile : String) {
    val out = new java.io.PrintWriter(outFile)
    out.println("Identifier, Same As, Definitions, Domain, Range")
    val props = ontology.objectProperties.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[ObjectProperty])

    for(prop <- props) {
      val uri = prop.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",,\"" +
          descriptions.getOrElse(uri.drop(ms.length), Nil).toSet.mkString("; ").replaceAll("\"", "\\\\\"") +
          "\"," + prop.domain.map(u => dropIfMs(u.name,ms)).mkString(" or ") + "," + 
          prop.range.map(u => dropIfMs(u.name,ms)).mkString(" or "))
      } else {
        out.println("," + uri +",\"\"," + prop.domain.map(u => dropIfMs(u.name,ms)).mkString(" or ") + "," + 
          prop.range.map(u => dropIfMs(u.name,ms)).mkString(" or "))
      }
    }
    out.flush
    out.close
  }
  def writeDataProperties(ontology : Ontology, descriptions : Map[String, List[String]], ms : String, outFile : String) {
      val out = new java.io.PrintWriter(outFile)
    out.println("Identifier, Same As, Definitions, Domain, Range")
    val props = ontology.dataProperties.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[DataProperty])

    for(prop <- props) {
      val uri = prop.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",,\"" +
          descriptions.getOrElse(uri.drop(ms.length), Nil).toSet.mkString("; ").replaceAll("\"", "\\\\\"") +
          "\"," + prop.domain.map(u => dropIfMs(u.name,ms)).mkString(" or ") + "," + 
          prop.range.toString.drop(prop.range.toString.indexOf("#") + 1))
      } else {
        out.println("," + uri +",\"\"," + prop.domain.map(u => dropIfMs(u.name,ms)).mkString(" or ") + "," + 
          prop.range.toString.drop(prop.range.toString.indexOf("#") + 1))
      }
    }
    out.flush
    out.close

  
  }
  def writeIndividual(ontology : Ontology, ms : String, outFile : String) {
    val out = new java.io.PrintWriter(outFile)
    out.println("Identifier, Same As, Type")
    val indivs = ontology.individuals.toSeq.sortBy(_.toString).foldLeft(Seq[Entity]())(dedupe).map(_.asInstanceOf[Individual])
    for(indiv <- indivs) {
      val uri = indiv.name.toString
      if(uri.startsWith(ms)) {
        out.println(uri.drop(ms.length) + ",," + dropIfMs(indiv.`type`.name,ms))
      } else {
        out.println("," + uri + "," + dropIfMs(indiv.`type`.name,ms))
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
      for(d <- elem.domain if d.name.toString.startsWith(prefix)) {
        for(r <- elem.range if r.name.toString.startsWith(prefix)) {
        out.println("%s -> %s [ label=\"%s\" ];" format (d.name.toString.drop(prefix.length),
                                                         r.name.toString.drop(prefix.length),
                                                         shorten(uri)))
        }
      }
    }
    out.println("}")
    out.flush()
    out.close()
  }

  def main(args : Array[String]) {
    if(args.length < 5) {
      System.err.println("Usage:\nsbt run \"modelClass csv|owl|dot descriptions prefix outFile\"\n")
      System.exit(-1)
    }
    val model : Ontology = args(0) match {
      case "metashare" => 
        OntologyExtractor(models.Metashare, 
          "http://www.ilsp.gr/META-XMLSchema","resourceInfo").ontology
      case _ =>
        System.err.println("Unknown model")
        System.exit(-1)
        null
    }

    val descriptions = io.Source.fromFile(args(2)).getLines.map(_.split("\t")).toSeq.groupBy(_(0)).mapValues(_.toList.map(_.drop(1).mkString("\t")))
    val prefix = args(3)
    val outFile = args(4)

    args(1) match {
      case "dot" =>
        writeDot(model, outFile, prefix)
      case "csv" =>
        writeClasses(model, descriptions, prefix, outFile + ".classes.csv")
        writeObjProperties(model, descriptions, prefix, outFile + ".objprops.csv")
        writeDataProperties(model, descriptions, prefix, outFile + ".dataprops.csv")
        writeIndividual(model, prefix, outFile + ".indivs.csv")
      case "owl" =>
        val out = new java.io.PrintWriter(outFile)
        for(e <- model.entities if e.name.toString.startsWith(prefix)) {
          val eName = e.name.toString.drop(prefix.length)
          val EName = eName(0).toLower + eName.drop(1)
          if(eName.startsWith("dialect")) {
            println(eName)
          }
          descriptions.get(eName) match {
            case Some(ds) =>
              if(eName.startsWith("dialect")) {
                println(ds.headOption)
              }
              e.definition = ds.headOption
            case _ =>
              descriptions.get(EName) match {
                case Some(ds) =>
                  e.definition = ds.headOption
                case _ => {}
              }
          }
        }
        out.println(model.toXML(prefix))
        out.flush
        out.close
    }
  }
}

case class NotSchematic(msg : String = "", cause : Throwable = null) extends 
  RuntimeException(msg, cause)
