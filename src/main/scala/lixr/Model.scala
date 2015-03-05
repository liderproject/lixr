package eu.liderproject.lixr

import java.net.URI
import scala.language.dynamics

trait Model {
  val xsd = Namespace("http://www.w3.org/2001/XMLSchema#")

  // The map containing the data
  private var handlers = collection.mutable.Map[(Option[String],String),Seq[(Request,Seq[Generator])]]()

  case class Namespace(__url__ : String) extends Dynamic {
    private val __names__ = collection.mutable.Set[String]()
    def selectDynamic(name : String) = {
      __names__ += name
      NodeRequest(Some(__url__), FixedTextGenerator(name))
    }
    def +(name : PlainTextGenerator) = NodeRequest(Some(__url__), name)
    def +(name : String) = {
      __names__ += name
      NodeRequest(Some(__url__), FixedTextGenerator(name))
    }
    def getAllNames = __names__.toSet
  }

  trait Request {
    def lookup : (Option[String],String)
    def --> (foo : Generator*) {
      if(!handlers.contains(lookup)) {
        handlers(lookup) = Seq((this,foo.toSeq))
      } else {
        handlers(lookup) :+= (this,foo.toSeq)
      }
    }
    def when(cond : Condition[Any]) = ConditionRequest(this, cond)
    def \(request : Request) = ChainRequest(this, request)
    def att(nr : NodeRequest) = AttributeContentGenerator(this, nr)
    def att(s : PlainTextGenerator) = AttributeContentGenerator(this, NodeRequest(None,s))
    def att(s : String) = AttributeContentGenerator(this, NodeRequest(None,FixedTextGenerator(s)))
    def isEmpty = NegationCondition(ExistenceCondition(this))
  }

  case class NodeRequest(namespace : Option[String], name : PlainTextGenerator) extends Request {
    def >(nr : NodeRequest) = {
      //println("Object Property:" + this.toURI(_.toString))
      //println("Individual:" + nr.toURI(_.toString))
      OTripleGenerator(this, nr)
    }
    def >(gen : TextGenerator) = {
      //println("Data Property:" + this.toURI(_.toString))
      DTripleGenerator(this, gen)
    }
    def >(ng : NodeGenerator) = {
      //println("Property:" + this.toURI(_.toString))
      NTripleGenerator(this, ng)
    }
    def >[G <: Generator](cg : ConditionalGenerator[G]) = {
      ConditionalTripleGenerator(this, cg)
    }
    def >(value : Boolean) = {
      DTripleGenerator(this, (text(value.toString) ^^ xsd.boolean))
    }
    def <(nr : NodeRequest) = IOTripleGenerator(this, nr)
    def <(ng : NodeGenerator) = INTripleGenerator(this, ng)
    lazy val lookup = name match {
      case FixedTextGenerator(name) => (namespace,name)
      case _ => throw new RuntimeException("Looking up non-fixed namespace")
    }
    def toURI(realize : PlainTextGenerator => String) = URI.create(namespace.getOrElse("") + realize(name))
  }

  object NodeRequest {
    def resolveStringAsRequest(bar : String) = NodeRequest(None,FixedTextGenerator(bar))
    def resolveStringAsRequest(bar : Symbol) = NodeRequest(None,FixedTextGenerator(bar.name))
  }

  case class ChainRequest(first : Request, second : Request) extends Request {
    def lookup = first.lookup
  }

  case class ConditionRequest(req : Request, cond : Condition[Any]) extends Request {
    def lookup = req.lookup
  }

  object current extends Request {
    def lookup = (None,"")
  }

  sealed trait Generator {
    def ++(g : Generator) = GeneratorList(this,g)
    def flatten = Seq(this)
  }

  sealed trait TextGenerator extends Generator 

  trait PlainTextGenerator extends TextGenerator {
    def ^^(nodeRequest : NodeRequest) = TypedTextGenerator(this, nodeRequest)
    def ^^(text : PlainTextGenerator) = TypedTextGenerator(this, NodeRequest(None, text))
    def ^^(text : String) = TypedTextGenerator(this, NodeRequest(None, FixedTextGenerator(text)))
    def @@(text : PlainTextGenerator) = LangTextGenerator(this, text)
    def @@(text : String) = LangTextGenerator(this, FixedTextGenerator(text))
    def ===(target : PlainTextGenerator) : Condition[PlainTextGenerator] = EqualityCondition(this,target)
    def ===(target : String) : Condition[PlainTextGenerator] = EqualityCondition(this, FixedTextGenerator(target))
    def !==(target : PlainTextGenerator) : Condition[PlainTextGenerator] = InequalityCondition(this,target)
    def !==(target : String) : Condition[PlainTextGenerator] = InequalityCondition(this, FixedTextGenerator(target))
    def matches(regex : String) : Condition[PlainTextGenerator] = RegexCondition(this, regex)
    def exists : Condition[PlainTextGenerator] = ExistenceCondition(this)
    def +:(s : String) : PlainTextGenerator = AppendTextGenerator(Some(s),this,None)
    def :+(s : String) : PlainTextGenerator = AppendTextGenerator(None, this, Some(s))
    def or(alternative : PlainTextGenerator) = TextAltGen(this, alternative)
    def substring(from : Int, to : Int) = SubstringTextGenerator(this, from, to)
  }

  case class FixedTextGenerator(str : String) extends PlainTextGenerator {
    override def toString = str
  }

  case class TypedTextGenerator(str : PlainTextGenerator, typ : NodeRequest) extends TextGenerator 

  case class LangTextGenerator(str : PlainTextGenerator, lang : PlainTextGenerator) extends TextGenerator

  case class XMLTextGenerator(node : Request) extends TextGenerator

  case class OTripleGenerator(prop : NodeRequest, value : NodeRequest) extends Generator 

  case class DTripleGenerator(prop : NodeRequest, value : TextGenerator) extends Generator 

  case class NTripleGenerator(prop : NodeRequest, value : NodeGenerator) extends Generator 

  case class IOTripleGenerator(prop : NodeRequest, value : NodeRequest) extends Generator 

  case class INTripleGenerator(prop : NodeRequest, value : NodeGenerator) extends Generator 

  case class ConditionalTripleGenerator[G <: Generator](prop : NodeRequest, value : ConditionalGenerator[G]) extends Generator

  case class AttributeGenerator(name : NodeRequest, text : TextGenerator) extends Generator

  case class RecursiveGenerator(request : Request) extends Generator

  case class FailGenerator(message : Seq[TextGenerator]) extends Generator

  case class CommentGenerator(message : Seq[TextGenerator]) extends Generator

  case class NodeGenerator(about : TextGenerator, body : Seq[Generator]) extends Generator

  case class TextAltGen(primary : TextGenerator, alt : PlainTextGenerator) extends PlainTextGenerator

  case class ContentGenerator(node : Request) extends PlainTextGenerator

  case class AttributeContentGenerator(node : Request, att : NodeRequest) extends PlainTextGenerator

  case class FragmentGenerator(frag : Seq[TextGenerator]) extends PlainTextGenerator

  case class URIGenerator(uri : PlainTextGenerator) extends PlainTextGenerator

  case class GeneratorList(first : Generator, second : Generator) extends Generator {
    override def flatten = first.flatten ++ second.flatten
  }

  case class SetVariable(name : String, value : PlainTextGenerator, context : Seq[Generator]) extends Generator

  case class GetVariable(name : String) extends PlainTextGenerator

  case class AppendTextGenerator(left : Option[String], generator : PlainTextGenerator, right : Option[String]) extends PlainTextGenerator {
    override def +:(s : String) : PlainTextGenerator = AppendTextGenerator(Some(left.getOrElse("") + s), generator, right)
    override def :+(s : String) : PlainTextGenerator = AppendTextGenerator(left, generator, Some(right.getOrElse("") + s))
  }

  case class ConcatTextGenerator(generators : Seq[PlainTextGenerator]) extends PlainTextGenerator

  case class SubstringTextGenerator(generator : PlainTextGenerator, to : Int, from : Int) extends PlainTextGenerator

  case class ForGenerator(req : Request, body : Seq[Generator]) extends Generator

  case class ConditionalGenerator[G <: Generator](condition : Condition[Any], result : Seq[G], otherwise : Option[G]) extends Generator {
    def or(condition2 : Condition[Any])(result2 : G*) = 
      ConditionalGenerator(condition, result, 
        Some(ConditionalGenerator(condition2, result2, None)))
    def otherwise(result2 : G) = 
      ConditionalGenerator(condition, result, Some(result2))
  }

  trait Condition[+A] {
    def check(resolve : A => Option[Any]) : Boolean
    def and[C >: A](c2 : Condition[C]) : Condition[C] = ConjunctionCondition(this, c2)
    def or[C >: A](c2 : Condition[C]) : Condition[C] = DisjunctionCondition(this, c2)
  }

  def not[A](c : Condition[A]) = NegationCondition(c)

  case class EqualityCondition[A](lhs : A, rhs : A) extends Condition[A] {
    def check(resolve : A => Option[Any]) = {
      resolve(lhs) == resolve(rhs)
    }
  }

  case class InequalityCondition[A](lhs : A, rhs : A) extends Condition[A] {
    def check(resolve : A => Option[Any]) = resolve(lhs) != resolve(rhs)
  }

  case class RegexCondition(target : PlainTextGenerator, pattern : String) 
      extends Condition[PlainTextGenerator] {
    def check(resolve : PlainTextGenerator => Option[Any]) = resolve(target) match {
      case Some(s : String) => s.matches(pattern)
      case Some(_) => false
      case None => false
    }
  }

  case class ExistenceCondition[A](target : A) extends Condition[A] {
    def check(resolve : A => Option[Any]) = resolve(target) != None
  }

  case class ConjunctionCondition[A](left : Condition[A], right : Condition[A]) extends Condition[A] {
    def check(resolve : A => Option[Any]) = 
      left.check(resolve) && right.check(resolve)
  }

  case class DisjunctionCondition[A](left : Condition[A], right : Condition[A]) extends Condition[A] {
    def check(resolve : A => Option[Any]) = 
      left.check(resolve) || right.check(resolve)
  }

  case class NegationCondition[A](cond : Condition[A]) extends Condition[A] {
    def check(resolve : A => Option[Any]) =
      !cond.check(resolve)
  }

  def getHandlers(reqURI : (Option[String],String)) = handlers.getOrElse(reqURI,Seq())
  def checkAtt(name : NodeRequest, text : TextGenerator) = AttributeGenerator(name, text)
  def checkAtt(name : NodeRequest, str : String) = AttributeGenerator(name, FixedTextGenerator(str))
  def checkAtt(name : String, text : TextGenerator) = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),text)
  def checkAtt(name : String, str : String) = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),FixedTextGenerator(str))
  def checkAtt(name : Symbol, text : TextGenerator) = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),text)
  def checkAtt(name : Symbol, str : String) = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),FixedTextGenerator(str))

  def text(value : String) = FixedTextGenerator(value)

  def handle(nodes : Request) = RecursiveGenerator(nodes)
  def handle(name : Symbol) = RecursiveGenerator(NodeRequest.resolveStringAsRequest(name))

  def fail(message : TextGenerator*) = FailGenerator(message)
  def fail(message : String) = FailGenerator(FixedTextGenerator(message) :: Nil)

  def comment(message : TextGenerator*) = CommentGenerator(message)
  def comment(message : String) = CommentGenerator(FixedTextGenerator(message) :: Nil)

  def node(about : TextGenerator)(triples : Generator*) = NodeGenerator(about, triples)
  def node(about : String)(triples : Generator*) = NodeGenerator(FixedTextGenerator(about), triples)

  def content = ContentGenerator(current)

  def content(nr : Request) = ContentGenerator(nr)

  def xmlContent = XMLTextGenerator(current)

  def xmlContent(nr : Request) = XMLTextGenerator(nr)

  def uuid = FixedTextGenerator(java.util.UUID.randomUUID().toString)
  
  def frag(frags : TextGenerator*) = FragmentGenerator(frags)
  def frag(s : String) = FragmentGenerator(FixedTextGenerator(s) :: Nil)

  def uri(uri : PlainTextGenerator) = URIGenerator(uri)
  def uri(s : String) = URIGenerator(FixedTextGenerator(s))

  def prop(s : String) = NodeRequest.resolveStringAsRequest(s)

  def att(name : NodeRequest) = current.att(name)
  def att(s : String) = current.att(s)

  def set(name : String, value : PlainTextGenerator)(context : Generator*) = SetVariable(name, value, context)
  def set(name : String, value : String)(context : Generator*) = SetVariable(name, FixedTextGenerator(value), context)

  def get(name : String) = GetVariable(name)

  def when[G <: Generator](condition : Condition[Any])(result : G*) = ConditionalGenerator[G](condition, result, None)

  def forall(req : Request)(body : Generator*) = ForGenerator(req, body)
  def forall(s : String)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s), body)
  def forall(s : Symbol)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s.name), body)

  def concat(ptgs : PlainTextGenerator*) = ConcatTextGenerator(ptgs)

  val rdf_type = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
  val a = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
    
  implicit class StringPimps(s : String) {
    def --> (foo : Generator*) = NodeRequest.resolveStringAsRequest(s).-->(foo:_*)
    def when (condition : Condition[Any]) = NodeRequest.resolveStringAsRequest(s).when(condition)
  }
  
  implicit class SymbolPimps(s : Symbol) {
    def --> (foo : Generator*) = NodeRequest.resolveStringAsRequest(s.name).-->(foo:_*)
    def when(cond : Condition[Any]) = NodeRequest.resolveStringAsRequest(s.name).when(cond)
  }

}

sealed trait GenResult {
  def asString : StringGenResult
}

trait StringGenResult extends GenResult {
  def asString = this
  def str : String
}

case class LiteralGenResult(val str : String) extends StringGenResult

case class LangStringResult(val str : String, lang : String) extends StringGenResult

case class TypedStringResult(val str : String, typ : URIGenResult) extends StringGenResult

case class URIGenResult(uri : URI) extends GenResult {
  lazy val asString = LiteralGenResult(uri.toString)
}

trait TripleResult extends GenResult {
  lazy val asString = LiteralGenResult("")
  protected def reformat(string : String) = string.replaceAll("\\\\","\\\\\\\\").replaceAll("\\n","\\\\n").replaceAll("\\t","\\\\t").replaceAll("\"","\\\\\"")
}

case class ObjTripleResult(subj : URI, property : URI, obj : URI) extends TripleResult {
  override def toString = "<%s> <%s> <%s> ." format (subj.toString, property.toString, obj.toString)
}

case class LitTripleResult(subj : URI, property : URI, obj : String) extends TripleResult {
  override def toString = "<%s> <%s> \"%s\" ." format (subj.toString, property.toString, reformat(obj))
}

case class LLTripleResult(subj : URI, property : URI, obj : String, lang : String) extends TripleResult {
  override def toString = "<%s> <%s> \"%s\"@%s ." format (subj.toString, property.toString, 
    reformat(obj), lang)
}

case class TLTripleResult(subj : URI, property : URI, obj : String, datatype : URI) extends TripleResult {
  override def toString = "<%s> <%s> \"%s\"^^<%s> ." format (subj.toString, property.toString, 
    reformat(obj), datatype.toString)
}

case class CommentGenResult(message : String) extends GenResult {
  lazy val asString = LiteralGenResult("")
}

case class FailGenResult(message : String) extends GenResult {
  lazy val asString = LiteralGenResult("")
}
