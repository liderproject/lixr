package eu.liderproject.lixr

import java.io.{InputStream,Reader}
import java.net.URI
import scala.xml._

class DOMGenerator {
  private val xmlLiteral = URIGenResult(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))

  case class State[+N <: Node](elem : N, model : Model, node : Option[URI], vars : Map[String,String])

  def process(state : State[Elem]) : Seq[GenResult] = {
    import state._
    val name = (Option(elem.namespace),elem.label)
    val handlers = model.getHandlers(name)
    if(handlers.isEmpty) {
      System.err.println("No handlers for node " + elem.label)
    }
    val applicable = handlers.find {
      case (r, g) => 
        verify(r, state)
    }
    applicable match {
      case Some((_, g)) => handle(g, state)
      case None => 
        if(!handlers.isEmpty) {
          System.err.println("No applicable handler for node " + elem.label)
        }
        Seq()
    }
  }

  def verify(req : Model#Request, state : State[Elem]) : Boolean = {
    import state._
    req match {
      case model.NodeRequest(Some(n), name) => elem.namespace == n && elem.label == name
      case model.NodeRequest(None, name) => elem.namespace == null && elem.label == name
      case model.ChainRequest(first, second) => throw new UnsupportedOperationException("Sorry no chain request on left hand side")
      case model.ConditionRequest(r, cond) => verify(r, state) && cond.check { gen =>
        genStringOpt(gen, state)
      }
      case model.current => false
    }
  }

  def locateNode(req : Model#Request, state : State[Node]) : Seq[Node] = {
    import state._
    elem match {
      case e : Elem => locate(req, State(e, model, node, vars))
      case _ => req match {
        case model.current => Seq(elem)
        case model.ConditionRequest(r, cond) => locateNode(r,state) filter {
          n2 => cond.check { gen =>
            genStringOpt(gen, state)
          }
        }
        case _ => Seq()
      }
    }
  }

  def locate(req : Model#Request, state : State[Elem]) : Seq[Node] = {
    import state._
    req match {
      case model.NodeRequest(Some(n), name) => (elem \ name) filter { e => n == e.namespace }
      case model.NodeRequest(None, name) => (elem \ name) filter { e => e.namespace == null }
      case model.ChainRequest(first, second) => locate(first, state) flatMap { 
        e => locateNode(second, State(e, model, node, vars))
      }
      case model.ConditionRequest(r, cond) => locate(r, state) filter {
        elem => cond.check { gen =>
          genStringOpt(gen, State(elem, model, node, vars))
        }
      }
      case model.current => Seq(elem)
    }
  }

  def handle(gens : Seq[Model#Generator], state : State[Elem]) : Seq[GenResult] = {
    gens.flatMap(gen => handleOne(gen, state))
  }

  def genString(t : Model#TextGenerator, state : State[Elem]) : String = {
    import state._
    t match {
      case ttg : model.TypedTextGenerator => throw new RuntimeException("Nested type")
      case ltg : model.LangTextGenerator => throw new RuntimeException("Nested type")
      case xtg : model.XMLTextGenerator => throw new RuntimeException("Nested type")
      case _ => genStringOpt(t,state).getOrElse("")
    }
  }

  def genStringOpt(t : Model#TextGenerator, state : State[Node]) : Option[String] = {
    import state._
    t match {
      case model.FixedTextGenerator(s) => Some(s)
      case ttg : model.TypedTextGenerator => None
      case ltg : model.LangTextGenerator => None
      case xtg : model.XMLTextGenerator => None
      case model.TextAltGen(p, alt) => genStringOpt(p,state) match {
        case None => genStringOpt(alt, state)
        case Some(s) => Some(s)
      }
      case model.ContentGenerator(n) => 
        val t = locateNode(n, state).map(_.text).mkString("")
        if(t == "") {
          None
        } else {
          Some(t)
        }
      case model.AttributeContentGenerator(n, model.NodeRequest(Some(ns),name)) => 
        (locateNode(n, state).flatMap { n =>
          n.attribute(ns,name).getOrElse(Seq()).map(_.text).mkString("")
        }) match {
          case Seq() => None
          case xs => Some(xs.mkString(""))
        }
      case model.AttributeContentGenerator(n, model.NodeRequest(None,name)) => 
        (locateNode(n, state).flatMap { n => 
          n.attribute(name).getOrElse(Seq()).map(_.text)
        }) match {
          case Seq() => None
          case xs => Some(xs.mkString(""))
        }
      case model.FragmentGenerator(frag) => 
        val fragText = (frag.flatMap { f =>
          genStringOpt(f, state)
        }).mkString("")
        node match {
          case Some(uri) =>
            Some(new URI(uri.getScheme(), uri.getSchemeSpecificPart(), fragText).toString)
          case None =>
            Some(URI.create("#"+fragText).toString)
        }
      case model.URIGenerator(uri) => genStringOpt(uri, state)
      case model.GetVariable(name) => vars.get(name)
      case model.AppendTextGenerator(left, generator, right) =>
        // Assume left or right is Some
        Some(left.getOrElse("") + genStringOpt(generator, state).getOrElse("") + right.getOrElse(""))
      case x => throw new UnsupportedOperationException("This is an error %s was generated please email john@mccra.e" format x.toString)
    }
  }


  def handleOne(gen : Model#Generator, state : State[Elem]) : Seq[GenResult] = {
    import state._

    def res2str(s : model.PlainTextGenerator) = {
      handleOne(s, state).headOption.getOrElse(throw new RuntimeException("Not string-like result")).asString.str 
    }

    gen match {
      case model.GeneratorList(head, tail) => 
        handleOne(head, state) ++ handleOne(tail, state)
      case model.FixedTextGenerator(str) => Seq(LiteralGenResult(str))
      case model.TypedTextGenerator(str, datatype) => Seq(TypedStringResult(
        res2str(str),
        URIGenResult(URI.create(genString(datatype, state)))))
      case model.LangTextGenerator(str, lang) => Seq(LangStringResult(
        res2str(str),
        res2str(lang)
      ))
      case model.URIGenerator(uri) =>
        genStringOpt(uri, state) match {
          case Some(s) => Seq(URIGenResult(URI.create(s)))
          case None => Seq()
        }
      case model.XMLTextGenerator(req) => 
        Seq(TypedStringResult(
          (locate(req, state) map { elem =>
            scala.xml.Utility.trim(elem).toString.replaceAll("\"","\\\\\"")
          }).mkString(""), xmlLiteral
          ))
      case t : model.TextGenerator => genStringOpt(t, state) match {
        case Some(s) => Seq(LiteralGenResult(s))
        case None => Seq()
      }
      case model.OTripleGenerator(nr1, nr2) => node match {
        case Some(s) => 
          Seq(ObjTripleResult(
            s, nr1.toURI, nr2.toURI))
        case None => throw new RuntimeException("No current node")
      }
      case model.DTripleGenerator(prop, value) => node match {
        case Some(s) => 
          handleOne(value, state) map { 
            case LiteralGenResult(o) => LitTripleResult(
              s, prop.toURI, o)
            case LangStringResult(o, l) => LLTripleResult(
              s, prop.toURI, o, l)
            case TypedStringResult(o, t) => TLTripleResult(
              s, prop.toURI, o, t.uri)
            case URIGenResult(u) => ObjTripleResult(
              s, prop.toURI, u)
            case _ => throw new RuntimeException("Unexpected result type in triple generation")
        }
        case None => throw new RuntimeException("No current node")
      }
      case model.NTripleGenerator(prop, ng @ model.NodeGenerator(about, body)) => node match {
        case Some(s) => 
          val obj = URI.create(genString(about,state))
          ObjTripleResult(
            s, prop.toURI, obj) +: handleOne(ng, state)
        case None => throw new RuntimeException("No current node")
      }
      case model.IOTripleGenerator(prop, subj) => node match {
        case Some(s) => 
          Seq(ObjTripleResult(
            subj.toURI, prop.toURI, s))
        case None => throw new RuntimeException("No current node")
      }
      case model.INTripleGenerator(prop, ng @ model.NodeGenerator(about, body)) => node match {

        case Some(s) => 
          val obj = URI.create(genString(about,state))
          ObjTripleResult(
            obj, prop.toURI, s) +: handleOne(ng, state)
        case None => throw new RuntimeException("No current node")
      }
      case model.AttributeGenerator(name, text) => Seq()
      case model.RecursiveGenerator(request) => 
        locateNode(request, state) flatMap { 
          case e : Elem => process(State(e, model, node, vars)) filter {
            case x : TripleResult => true
            case x : CommentGenResult => true 
            case x : FailGenResult => true
            case _ => false
          }
          case n : Node => throw new RuntimeException("Handle resolved to non elements!?")
        }
      case model.FailGenerator(messages) => Seq(FailGenResult(messages.flatMap { message =>
        genStringOpt(message, state)
      }.mkString("")))
      case model.CommentGenerator(messages) => Seq(CommentGenResult(messages.flatMap { message =>
        genStringOpt(message, state)
      }.mkString("")))
      case model.NodeGenerator(about, body) => 
        val uri = URI.create(genString(about, state))
        body.flatMap { g =>
          handleOne(g, State(elem, model, Some(uri), vars))
        }
      case model.SetVariable(name, value, context) => 
        genStringOpt(value, state) match {
          case Some(v) =>
            handle(context, State(elem, model, node, vars + (name -> v)))
          case None => 
            throw new RuntimeException("Variable %s requested but not set" format name)
        }
      case model.ConditionalGenerator(cond, value, otherwise) =>
        if(cond.check(genStringOpt(_, state))) {
          handleOne(value, state)
        } else {
          otherwise match {
            case Some(o) => handleOne(o, state)
            case None => Seq()
          }
        }
      case model.ForGenerator(req, body) =>
        locate(req,state) flatMap { 
          case n : Elem => handle(body, State(n, model, node, vars))
          case _ => Seq()
        }
      case x => throw new UnsupportedOperationException("This is an error %s was generated please email john@mccra.e" format x.toString)
    }
  }

  def read(in : InputStream, model : Model) = {
    process(State(XML.load(in), model, None, Map()))
  }

  def read(in : Reader, model : Model) = {
    process(State(XML.load(in), model, None, Map()))
  }
}
