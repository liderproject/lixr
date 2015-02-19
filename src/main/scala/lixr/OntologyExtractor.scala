package eu.liderproject.lixr

import java.net.URI

trait Entity {
  def name : URI
  def toXML : String
}

case class Class(name : URI) extends Entity {
  def toXML = "  <owl:Class rdf:about=\"%s\"></owl:Class>" format (name)
}

object TopClass extends Class(URI.create("")) {
  override def toXML = ""
}

trait Property[A] extends Entity {
  def domain : Class
  def range : A
  protected def domStr = if(domain == TopClass) {
    "" 
  } else {
    "    <rdfs:domain rdf:resource=\"%s\"/>\n" format(domain.name)
  }

}

case class ObjectProperty(name : URI, domain : Class, range : Class) extends Property[Class] {
  def toXML = """  <owl:ObjectProperty rdf:about="%s">
%s    <rdfs:range rdf:resource="%s"/>
  </owl:ObjectProperty>""" format (name, domStr, range.name)
}

case class DataProperty(name : URI, domain : Class, range : URI) extends Property[URI] {
  def toXML = """  <owl:DatatypeProperty rdf:about="%s">
%s    <rdfs:range rdf:resource="%s"/>
  </owl:DatatypeProperty>""" format (name, domStr, range)
}

case class Individual(name : URI, `type` : Class) extends Entity {
  private val qname = "(.*?)(\\w+)$".r
  def toXML = {
    val qname(pre, suf) = `type`.name.toString
    """  <ns:%s rdf:about="%s" xmlns:ns="%s"/>""" format (suf, name, pre)
  }
}
    

case class Ontology(entities : Set[Entity]) {
  def classes = entities.filter(_.isInstanceOf[Class])
  def properties = entities.filter(_.isInstanceOf[Property[_]])
  def objectProperties = entities.filter(_.isInstanceOf[ObjectProperty])
  def dataProperties = entities.filter(_.isInstanceOf[DataProperty])
  def individuals = entities.filter(_.isInstanceOf[Individual])
  def toXML = """<rdf:RDF
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  
  <rdfs:Datatype rdf:about="http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"/>
  
""" + 
  entities.map(_.toXML).mkString("\n\n") + """</rdf:RDF>"""

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
          clazz.getOrElse(TopClass),
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
         clazz.getOrElse(TopClass),
         c),
       Individual(
         nr2uri(value),
         c),
       c)
    case NTripleGenerator(prop, NodeGenerator(_, body)) =>
      val (rangeClass, velems) = handle(body)
      ObjectProperty(
        nr2uri(prop),
        clazz.getOrElse(TopClass),
        rangeClass.getOrElse(TopClass)
      ) +: velems
    case INTripleGenerator(prop, NodeGenerator(_, body)) =>
      val (domainClass, velems) = handle(body)
      ObjectProperty(
        nr2uri(prop),
        domainClass.getOrElse(TopClass),
        clazz.getOrElse(TopClass)
      ) +: velems

    case ConditionalGenerator(_, r, Some(o)) =>
      handle(clazz, r) ++ handle(clazz, o)
    case ConditionalGenerator(_, r, None) =>
      handle(clazz, r)
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
}

case class NotSchematic(msg : String = "", cause : Throwable = null) extends 
  RuntimeException(msg, cause)
