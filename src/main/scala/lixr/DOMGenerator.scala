package eu.liderproject.lixr

import java.io.{InputStream,Reader}
import java.net.URI
import scala.xml._

class DOMGenerator {
  private val xmlLiteral = URIGenResult(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))
  private val frags = collection.mutable.Set[String]()
  private val elem2frag = collection.mutable.Map[Node, String]()

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

//  def conditionEvalNode(x : Any, state : State[Node]) : Option[Any] = x match {
//    case gen : state.model.TextGenerator =>
//      genStringOpt(gen, state)
//    case req : state.model.Request =>
//      val nodes = locateNode(req, state)
//      if(!nodes.isEmpty) {
//        Some(nodes)
//      } else {
//        None
//      }
//  }
//
//  def conditionEval(x : Text, state : State[Elem]) : Option[Any] = x match {
//    case gen : state.model.TextGenerator =>
//      genStringOpt(gen, state)
//    case req : state.model.Request =>
//      val nodes = locate(req, state)
//      if(!nodes.isEmpty) {
//        Some(nodes)
//      } else {
//        None
//      }
//  }


  def verify(req : Model#Handleable, state : State[Elem]) : Boolean = {
    import state._
    req match {
      case model.NodeRequest(Some(n), name) => elem.namespace == n && elem.label == genString(name, state)
      case model.NodeRequest(None, name) => elem.namespace == null && elem.label == genString(name, state)
      case model.HandleableConditionRequest(r, cond) => verify(r, state) && cond.check(genStringOpt(_, state), locate(_, state))
      case _ => throw new RuntimeException("Shouldn't happen")
    }
  }

  def locateNode(req : Model#Request, state : State[Node]) : Seq[Node] = {
    import state._
    elem match {
      case e : Elem => locate(req, State(e, model, node, vars))
      case _ => req match {
        case model.current => Seq(elem)
        case model.ConditionRequest(r, cond) => locateNode(r,state) filter {
          n2 => cond.check(genStringOpt(_, state), locateNode(_, state))
        }
        case _ => Seq()
      }
    }
  }

  def locate(req : Model#Request, state : State[Elem]) : Seq[Node] = {
    import state._
    req match {
      case model.NodeRequest(Some(n), name) => (elem \ genString(name, state)) filter { e => n == e.namespace }
      case model.NodeRequest(None, name) => (elem \ genString(name, state)) filter { e => e.namespace == null }
      case model.ChainRequest(first, second) => locate(first, state) flatMap { 
        e => locateNode(second, State(e, model, node, vars))
      }
      case model.ConditionRequest(r, cond) => locate(r, state) filter {
        elem => cond.check(genStringOpt(_, state), locate(_, state))
      }
      case model.current => Seq(elem)
      case _ => throw new RuntimeException("Shouldn't happen")
    }
  }

  def handle(gens : Seq[Model#Generator], state : State[Elem]) : Seq[GenResult] = {
    gens.flatMap(gen => handleOne(gen, state))
  }

  def genString(t : Model#TextGenerator, state : State[Node]) : String = {
    import state._
    genStringOpt(t,state).getOrElse("")
  }

  def genStringOpt(t : Model#TextGenerator, state : State[Node]) : Option[String] = {
    import state._
    t match {
      case model.FixedTextGenerator(s) => Some(s)
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
          n.attribute(ns,genString(name, state)).getOrElse(Seq()).map(_.text).mkString("")
        }) match {
          case Seq() => None
          case xs => Some(xs.mkString(""))
        }
      case model.AttributeContentGenerator(n, model.NodeRequest(None,name)) => 
        (locateNode(n, state).flatMap { n => 
          n.attribute(genString(name, state)).getOrElse(Seq()).map(_.text)
        }) match {
          case Seq() => None
          case xs => Some(xs.mkString(""))
        }
      case model.FragmentGenerator(frag) => 
        val fragText = genStringOpt(frag, state).getOrElse("")
        node match {
          case Some(uri) =>
            val ssp = uri.getSchemeSpecificPart()
            elem2frag.get(state.elem) match {
              case Some(uniqFrag) => 
                Some(new URI(uri.getScheme(), ssp, uniqFrag).toString)
              case None => 
                // We are attempting to guarantee that the fragment is unique
                val uniqFrag = fragText + (Stream("") ++ Stream.from(2).map(_.toString)).
                  find(x => !frags.contains(ssp + fragText + x)).get
                frags.add(ssp + uniqFrag)
                elem2frag.put(state.elem, uniqFrag)
                Some(new URI(uri.getScheme(), ssp, uniqFrag).toString)
            }
          case None =>
            throw new IllegalArgumentException("No node")
        }
      case model.GetVariable(name) => vars.get(name)
      case model.AppendTextGenerator(left, generator, right) =>
        // Assume left or right is Some
        Some(left.getOrElse("") + genStringOpt(generator, state).getOrElse("") + right.getOrElse(""))
      case model.ConcatTextGenerator(tg1, tg2) =>
        genStringOpt(tg1, state).flatMap(x => genStringOpt(tg1, state).map(y => x + y))
      case model.TransformTextGenerator(ptg, forward, _) =>
        genStringOpt(ptg, state).map(forward)
      case x => throw new UnsupportedOperationException("This is an error %s was generated please email john@mccr.ae" format x.toString)
    }
  }

  def handleOne(gen : Model#DataValueGenerator, state : State[Elem]) : Seq[GenResult] = {
    import state._

    def res2str(s : model.TextGenerator) = {
      handleOne(s, state).headOption.getOrElse(throw new RuntimeException("Not string-like result")).asString.str 
    }

    gen match {
      case model.FixedTextGenerator(str) => Seq(LiteralGenResult(str))
      case model.TypedTextGenerator(str, datatype) => Seq(TypedStringResult(
        res2str(str),
        URIGenResult(datatype.toURI(genString(_,state)))))
      case model.LangTextGenerator(str, lang) => Seq(LangStringResult(
        res2str(str),
        res2str(lang)
      ))
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
      case _ => throw new RuntimeException("Wrong model")
    }

  }

  def handleOne(gen : Model#Generator, state : State[Elem]) : Seq[GenResult] = {
    import state._

    gen match {
      case model.GeneratorList(head, tail) => 
        handleOne(head, state) ++ handleOne(tail, state)
      case model.OTripleGenerator(nr1, nr2) => node match {
        case Some(s) => 
          Seq(ObjTripleResult(
            s, URI.create(genString(nr1, state)), nr2.toURI(genString(_,state))))
        case None => 
          
        throw new RuntimeException("No current node" + gen)
      }
      case model.DTripleGenerator(prop, value) => node match {
        case Some(s) => 
          val p = URI.create(genString(prop, state))
          handleOne(value, state) map { 
            case LiteralGenResult(o) => LitTripleResult(
              s, p, o)
            case LangStringResult(o, l) => LLTripleResult(
              s, p, o, l)
            case TypedStringResult(o, t) => TLTripleResult(
              s, p, o, t.uri)
            case URIGenResult(u) => ObjTripleResult(
              s, p, u)
            case _ => throw new RuntimeException("Unexpected result type in triple generation")
        }
        case None => throw new RuntimeException("No current node" + gen)
      }
      case model.NTripleGenerator(prop, ng @ model.NodeGenerator(about, body)) => node match {
        case Some(s) => 
          val obj = URI.create(genString(about,state))
          ObjTripleResult(
            s, URI.create(genString(prop, state)), obj) +: handleOne(ng, state)
        case None => throw new RuntimeException("No current node" + gen)
      }
      case model.ConditionalTripleGenerator(prop, cond) => node match {
        case Some(s) =>
          handleOne(cond, state) map { 
            case LiteralGenResult(o) => LitTripleResult(
              s, URI.create(genString(prop, state)), o)
            case LangStringResult(o, l) => LLTripleResult(
              s, URI.create(genString(prop, state)), o, l)
            case TypedStringResult(o, t) => TLTripleResult(
              s, URI.create(genString(prop, state)), o, t.uri)
            case URIGenResult(uri) => ObjTripleResult(
              s, URI.create(genString(prop, state)), uri)
            case other => FailGenResult("Non-literal returned by conditional")
          }
        case None =>
          throw new RuntimeException("No current node " + gen)
      }
      case model.IOTripleGenerator(prop, subj) => node match {
        case Some(s) => 
          Seq(ObjTripleResult(
            subj.toURI(genString(_,state)), URI.create(genString(prop, state)), s))
        case None => throw new RuntimeException("No current node" + gen)
      }
      case model.INTripleGenerator(prop, ng @ model.NodeGenerator(about, body)) => node match {

        case Some(s) => 
          val obj = URI.create(genString(about,state))
          ObjTripleResult(
            obj, URI.create(genString(prop, state)), s) +: handleOne(ng, state)
        case None => throw new RuntimeException("No current node" + gen)
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
      case model.FailGenerator(message) => Seq(FailGenResult(genString(message, state)))
      case model.CommentGenerator(message) => Seq(CommentGenResult(genString(message, state)))
      case model.MessageGenerator(message) => {
        System.err.println(genString(message, state))
        Seq()
      }
      case model.NodeGenerator(about, body) => 
        val uri = try {
          URI.create(genString(about, state).trim())
        } catch {
          case x : Exception =>
            System.err.println("Failed to generate node name @ %s" format (genString(about, state).toString))
            URI.create("err:" + java.util.UUID.randomUUID().toString)
        }
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
        if(cond.check(genStringOpt(_, state), locate(_, state))) {
          value.flatMap(handleOne(_, state))
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
      case model.TextContentGenerator(body) => Seq()
      case model.XMLContentGenerator(elem) => Seq()
      case x => throw new UnsupportedOperationException("This is an error %s was generated please email john@mccra.e" format x.toString)
    }
  }

  def read(in : InputStream, model : Model) = {
    if(System.getProperty("lixr.trim") == "true") {
      process(State(Utility.trim(XML.load(in)).asInstanceOf[Elem], model, None, Map()))
    } else {
      process(State(XML.load(in), model, None, Map()))
    }
  }

  def read(in : Reader, model : Model) = {
    if(System.getProperty("lixr.trim") == "true") {
      process(State(Utility.trim(XML.load(in)).asInstanceOf[Elem], model, None, Map()))
    } else {
      process(State(XML.load(in), model, None, Map()))
    }
  }
}
