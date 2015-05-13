package eu.liderproject.lixr

import java.net.URI
import scala.language.dynamics

/** The generic type of all models */
trait Model {
  /** The XSD namespace used for datatypes */
  val xsd = Namespace("http://www.w3.org/2001/XMLSchema#")

  // The map containing the data
  private var handlers = collection.mutable.Map[(Option[String],String),Seq[(Handleable,Seq[Generator])]]()

  /** A namespace abbreviation */
  case class Namespace(__url__ : String) extends Dynamic {
    private val __names__ = collection.mutable.Set[String]()
    def selectDynamic(name : String) = {
      __names__ += name
      NodeRequest(Some(__url__), FixedTextGenerator(name))
    }
    /** Generate a member of this namespace with a text generator */
    def +(name : TextGenerator) = {
      NodeRequest(Some(__url__), name)
    }
    /** Generate a member of this namespace with a text generator */
    def +(name : String) = {
      __names__ += name
      NodeRequest(Some(__url__), FixedTextGenerator(name))
    }
    /** Return all static names created in this namespace */
    def getAllNames = __names__.toSet
  }

  /** An XPath-like node lookup request */
  trait Request {
    /**
     * Generates a condition that is true if this request matches no nodes
     */
    def isEmpty : Condition[Any] = NegationCondition(ExistenceCondition(this))
    /**
     * Add a condition to this request
     */
    def when(cond : Condition[Any]) : Request = ConditionRequest(this, cond)
    /**
     * Creates a request for a direct child
     */
    def \(request : Request) : Request = ChainRequest(this, request)
    /**
     * Request a prefixed attribute of the current node
     * @param nr The prefixed attribute
     */
    def att(nr : NodeRequest) : TextGenerator = AttributeContentGenerator(this, nr)
    /**
     * Request an unprefixed attribute of the current node
     * @param s The unprefixed attribute
     */
    def att(s : TextGenerator) : TextGenerator = AttributeContentGenerator(this, NodeRequest(None,s))
    /**
     * Request an unprefixed attribute of the current node
     * @param s The unprefixed attribute
     */
    def att(s : String) : TextGenerator = AttributeContentGenerator(this, NodeRequest(None,FixedTextGenerator(s)))
  }

  /** A request that can be handled */
  trait Handleable {
    /** 
     * The primary lookup, as required for a handler
     */
    def lookup : (Option[String],String)
    /** 
     * Assign a handler to this request 
     * @param foo The actions to take if this request matches
     */
    def --> (foo : Generator*) {
      if(!handlers.contains(lookup)) {
        handlers(lookup) = Seq((this,foo.toSeq))
      } else {
        handlers(lookup) :+= (this,foo.toSeq)
      }
    }
  }

  /**
   * The 'base' request for a node by its name
   * @param namespace The namespace URL (not the shortened form)
   * @param name The node's name
   */
  case class NodeRequest(namespace : Option[String], name : TextGenerator) 
      extends Request with Handleable {
    /**
     * Generating an object property triple with the given value
     */
    def >(nr : NodeRequest) : Generator = {
      OTripleGenerator(this, nr)
    }
    /**
     * Generate a data property triple with the given value
     */
    def >(gen : DataValueGenerator) : Generator = {
      DTripleGenerator(this, gen)
    }
    /**
     * Generate an object property triple whose value is another node
     */
    def >(ng : NodeGenerator) : Generator = {
      NTripleGenerator(this, ng)
    }
    /**
     * Conditionally generate an object property
     * @deprecated This supports a syntax that is not necessary!
     */
    def >(cg : ConditionalGenerator) : Generator = {
      ConditionalTripleGenerator(this, cg)
    }
    /**
     * Generate an untyped string data property triple.
     * Note this does not generate the same as "foo"^^xsd.string
     */
    def >(value : String) : Generator = DTripleGenerator(this, FixedTextGenerator(value))
    /**
     * Generate an untyped boolean data property triple
     */
    def >(value : Boolean) : Generator = {
      DTripleGenerator(this, (text(value.toString) ^^ xsd.boolean))
    }
    /**
     * Generate an untyped int data property triple
     */
    def >(value : Int) : Generator = {
      DTripleGenerator(this, (text(value.toString) ^^ xsd.integer))
    }
    /**
     * Generate an untyped decimal data property triple
     */
    def >(value : Double) : Generator = {
      DTripleGenerator(this, (text(value.toString) ^^ xsd.decimal))
    }
    /**
     * Generate an object property triple with the active node as the object and 
     * the value as the subject (i.e., a 'backlink')
     */
    def <(nr : NodeRequest) : Generator = IOTripleGenerator(this, nr)
    /**
     * Generate an object property triple with the active node as the object and
     * the value as the subject, which is also a new triple block.
     */
    def <(ng : NodeGenerator) : Generator = INTripleGenerator(this, ng)
    lazy val lookup = name match {
      case FixedTextGenerator(name) => (namespace,name)
      case _ => throw new RuntimeException("Looking up non-fixed namespace")
    }
    override def when(cond : Condition[Any]) : Request with Handleable = HandleableConditionRequest(this, cond)
    /**
     * @deprecated Why does this exist here?
     */
    def toURI(realize : TextGenerator => String) = URI.create(namespace.getOrElse("") + realize(name))
  }

  object NodeRequest {
    /** Convert a string to a node request */
    def resolveStringAsRequest(bar : String) = NodeRequest(None,FixedTextGenerator(bar))
    /** Convert a symbol to a node request */
    def resolveStringAsRequest(bar : Symbol) = NodeRequest(None,FixedTextGenerator(bar.name))
  }

  private[lixr] case class ChainRequest(first : Request, second : Request) extends Request

  private[lixr] case class ConditionRequest(req : Request, cond : Condition[Any]) extends Request

  private[lixr] case class HandleableConditionRequest(req : Handleable with Request, 
    cond : Condition[Any]) extends Request with Handleable {
    def lookup = req.lookup
  }

  /** 
   * Symbol for the current node
   */
  object current extends Request

  /**
   * An object that generates something in the output
   */
  sealed trait Generator {
    /**
     * Apply this generator immediately followed by the next one in the list
     */
    def ++(g : Generator) = GeneratorList(this,g)
    /**
     * Convert this generator to a sequence of generator. If this generator is
     * not a GeneratorList this returns a list of one element
     */
    def flatten = Seq(this)
  }

  private[lixr] case class OTripleGenerator(prop : NodeRequest, value : NodeRequest) extends Generator 

  private[lixr] case class DTripleGenerator(prop : NodeRequest, value : DataValueGenerator) extends Generator 

  private[lixr] case class NTripleGenerator(prop : NodeRequest, value : NodeGenerator) extends Generator 

  private[lixr] case class IOTripleGenerator(prop : NodeRequest, value : NodeRequest) extends Generator 

  private[lixr] case class INTripleGenerator(prop : NodeRequest, value : NodeGenerator) extends Generator 

  private[lixr] case class ConditionalTripleGenerator(prop : NodeRequest, value : ConditionalGenerator) extends Generator

  private[lixr] case class AttributeGenerator(name : NodeRequest, text : TextGenerator) extends Generator

  private[lixr] case class RecursiveGenerator(request : Request) extends Generator

  private[lixr] case class FailGenerator(message : Seq[TextGenerator]) extends Generator

  private[lixr] case class CommentGenerator(message : Seq[TextGenerator]) extends Generator

  private[lixr] case class NodeGenerator(about : TextGenerator, body : Seq[Generator]) extends Generator

  private[lixr] case class GeneratorList(first : Generator, second : Generator) extends Generator {
    override def flatten = first.flatten ++ second.flatten
  }

  /**
   * Generate the object of a data property triple
   */
  sealed trait DataValueGenerator

  /**
   * Generate a piece of text
   */
  sealed trait TextGenerator extends DataValueGenerator {
    /**
     * Generate a typed RDF literal
     */
    def ^^(nodeRequest : NodeRequest) : DataValueGenerator = TypedTextGenerator(this, nodeRequest)
    /**
     * Generate a typed RDF literal
     */
    def ^^(text : TextGenerator) : DataValueGenerator = TypedTextGenerator(this, NodeRequest(None, text))
    /**
     * Generate a typed RDF literal
     */
    def ^^(text : String) : DataValueGenerator = TypedTextGenerator(this, NodeRequest(None, FixedTextGenerator(text)))
    /**
     * Generate a lang-tagged RDF literal
     */
    def @@(text : TextGenerator) : DataValueGenerator = LangTextGenerator(this, text)
    /**
     * Generate a lang-tagged RDF literal
     */
    def @@(text : String) : DataValueGenerator = LangTextGenerator(this, FixedTextGenerator(text))
    /**
     * Check equality of text generators
     */
    def ===(target : TextGenerator) : Condition[TextGenerator] = EqualityCondition(this,target)
    /**
     * Check equality of text generators
     */
    def ===(target : String) : Condition[TextGenerator] = EqualityCondition(this, FixedTextGenerator(target))
    /**
     * Check equality of text generators
     */
    def !==(target : TextGenerator) : Condition[TextGenerator] = InequalityCondition(this,target)
    /**
     * Check inequality of text generators
     */
    def !==(target : String) : Condition[TextGenerator] = InequalityCondition(this, FixedTextGenerator(target))
    /**
     * Check whether text generator matches a fixed regular expression
     */
    def matches(regex : String) : Condition[TextGenerator] = RegexCondition(this, regex)
    /**
     * Check whether text generator matches a non-empty string
     */
    def exists : Condition[TextGenerator] = ExistenceCondition(this)
    /**
     * Concatenate a text generator
     */
    def +:(s : String) : TextGenerator = AppendTextGenerator(Some(s),this,None)
    /**
     * Concatenate a text generator
     */
    def :+(s : String) : TextGenerator = AppendTextGenerator(None, this, Some(s))
    /**
     * If this text generator does not match a non-empty string use the alternative
     */
    def or(alternative : TextGenerator) = TextAltGen(this, alternative)
    /**
     * Transform the result of this text generator by replacing all matches of 
     * regex with regex2
     */
    def replace(regex : String, regex2 : String) = Model.this.replace(this, regex, regex2)
    /**
     * Take a substring of this text generator
     */
    def substring(from : Int, to : Int) = Model.this.substring(this, from, to)
  }

  case class FixedTextGenerator(str : String) extends TextGenerator {
    override def toString = str
  }

  private[lixr] case class TypedTextGenerator(str : TextGenerator, typ : NodeRequest) extends DataValueGenerator 

  private[lixr] case class LangTextGenerator(str : TextGenerator, lang : TextGenerator) extends DataValueGenerator

  private[lixr] case class XMLTextGenerator(node : Request) extends TextGenerator

  private[lixr] case class TextAltGen(primary : TextGenerator, alt : TextGenerator) extends TextGenerator

  private[lixr] case class ContentGenerator(node : Request) extends TextGenerator

  private[lixr] case class AttributeContentGenerator(node : Request, att : NodeRequest) extends TextGenerator

  private[lixr] case class FragmentGenerator(frag : Seq[TextGenerator]) extends TextGenerator

  private[lixr] case class URIGenerator(uri : TextGenerator) extends TextGenerator

  private[lixr] case class SetVariable(name : String, value : TextGenerator, context : Seq[Generator]) extends Generator

  private[lixr] case class GetVariable(name : String) extends TextGenerator

  private[lixr] case class AppendTextGenerator(left : Option[String], generator : TextGenerator, right : Option[String]) extends TextGenerator {
    override def +:(s : String) : TextGenerator = AppendTextGenerator(Some(left.getOrElse("") + s), generator, right)
    override def :+(s : String) : TextGenerator = AppendTextGenerator(left, generator, Some(right.getOrElse("") + s))
  }

  private[lixr] case class ConcatTextGenerator(generators : Seq[TextGenerator]) extends TextGenerator

  private[lixr] case class TransformTextGenerator(generator : TextGenerator, 
                                    forward : String => String,
                                    backward : String => String) extends TextGenerator

  /**
   * Transform the result of a text generator
   * @param generator The base string
   * @param forward The XML to RDF converter
   * @param backward The RDF to XML converter
   */
  def transform(generator : TextGenerator)(forward : String => String)
    (backward : String => String) = TransformTextGenerator(generator, forward, backward)

  /**
   * Transform the result of a text generator. 
   * @param generator The base string
   * @param forward The XML to RDF converter
   * @param backward The RDF to XML converter
   */
  def transform(generator : String)(forward : String => String)
    (backward : String => String) = TransformTextGenerator(FixedTextGenerator(generator), forward, backward)

  def replace(tg : TextGenerator, regex1 : String, regex2 : String) = 
    transform(tg)(_.replaceAll(regex1, regex2))(_.replaceAll(regex2, regex1))

  def substring(tg : TextGenerator, start : Int, end : Int) =
    transform(tg)(_.slice(start, end))(throw new UnsupportedOperationException())

//  case class SubstringTextGenerator(generator : TextGenerator, to : Int, from : Int) extends TextGenerator

  case class ForGenerator(req : Request, body : Seq[Generator]) extends Generator

  case class ConditionalGenerator(condition : Condition[Any], result : Seq[Generator], otherwize : Option[Generator]) extends Generator {
    def or(condition2 : Condition[Any])(result2 : Generator*) : ConditionalGenerator = otherwize match {
      case Some(o) => o match {
        case o : ConditionalGenerator =>
          ConditionalGenerator(condition, result, Some(o.or(condition2)(result2:_*)))
        case _ =>
          ConditionalGenerator(condition, result, 
            Some(ConditionalGenerator(condition2, result2, None)))
      }
      case None =>
        ConditionalGenerator(condition, result, 
          Some(ConditionalGenerator(condition2, result2, None)))
    }
    def otherwise(result2 : Generator) : ConditionalGenerator = otherwize match {
      case Some(o) => o match {
        case o : ConditionalGenerator =>
          ConditionalGenerator(condition, result, Some(o.otherwise(result2)))
        case _ =>
          ConditionalGenerator(condition, result, Some(result2))
      }
      case None =>
        ConditionalGenerator(condition, result, Some(result2))
    }
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

  case class RegexCondition(target : TextGenerator, pattern : String) 
      extends Condition[TextGenerator] {
    def check(resolve : TextGenerator => Option[Any]) = resolve(target) match {
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

  def uri(uri : TextGenerator) = URIGenerator(uri)
  def uri(s : String) = URIGenerator(FixedTextGenerator(s))

  def prop(s : String) = NodeRequest.resolveStringAsRequest(s)

  def att(name : NodeRequest) = current.att(name)
  def att(s : String) = current.att(s)

  def set(name : String, value : TextGenerator)(context : Generator*) = SetVariable(name, value, context)
  def set(name : String, value : String)(context : Generator*) = SetVariable(name, FixedTextGenerator(value), context)

  def get(name : String) = GetVariable(name)

  def when(condition : Condition[Any])(result : Generator*) = ConditionalGenerator(condition, result, None)

  def forall(req : Request)(body : Generator*) = ForGenerator(req, body)
  def forall(s : String)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s), body)
  def forall(s : Symbol)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s.name), body)

  def concat(ptgs : TextGenerator*) = ConcatTextGenerator(ptgs)

  val rdf_type = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
  val a = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
    
  implicit class StringPimps(s : String) {
    def --> (foo : Generator*) = NodeRequest.resolveStringAsRequest(s).-->(foo:_*)
    def when (condition : Condition[Any]) = NodeRequest.resolveStringAsRequest(s).when(condition)
    def ^^(nodeRequest : NodeRequest) = TypedTextGenerator(FixedTextGenerator(s), nodeRequest)
    def ^^(text : TextGenerator) = TypedTextGenerator(FixedTextGenerator(s), NodeRequest(None, text))
    def ^^(text : String) = TypedTextGenerator(FixedTextGenerator(s), NodeRequest(None, FixedTextGenerator(text)))
    def @@(text : TextGenerator) = LangTextGenerator(FixedTextGenerator(s), text)
    def @@(text : String) = LangTextGenerator(FixedTextGenerator(s), FixedTextGenerator(text))
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
