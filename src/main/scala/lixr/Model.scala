package eu.liderproject.lixr

import java.net.URI
import scala.language.dynamics
import scala.xml.Elem
import scala.xml.Node

/** The generic type of all models */
trait Model {
  /** The XSD namespace used for datatypes */
  val xsd = Namespace("http://www.w3.org/2001/XMLSchema#")

  // The map containing the data
  private var handlers = collection.mutable.Map[(Option[String],String),Seq[(Handleable,Seq[Generator])]]()
  /**
   * Get handlers that map a given namespace request
   */
  def getHandlers(reqURI : (Option[String],String)) = handlers.getOrElse(reqURI,Seq())

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
  sealed trait Request {
    /**
     * Generates a condition that is true if this request matches no nodes
     * @deprecated Prefer not(exists)
     */
    def isEmpty : Condition = NegationCondition(RequestExistenceCondition(this))
    /**
     * Generates a condition that is true if this request matches no nodes
     */
    def exists : Condition = RequestExistenceCondition(this)
     /**
     * Add a condition to this request
     */
    def when(cond : Condition) : Request = ConditionRequest(this, cond)
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
  sealed trait Handleable {
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
    private def prop = FixedTextGenerator(namespace.getOrElse("") + name)
    /**
     * Generating an object property triple with the given value
     */
    def >(nr : NodeRequest) : Generator = {
      OTripleGenerator(prop, nr)
    }
    /**
     * Generate a data property triple with the given value
     */
    def >(gen : DataValueGenerator) : Generator = {
      DTripleGenerator(prop, gen)
    }
    /**
     * Generate an object property triple whose value is another node
     */
    def >(ng : NodeGenerator) : Generator = {
      NTripleGenerator(prop, ng)
    }
    /**
     * Conditionally generate an object property
     * @deprecated This supports a syntax that is not necessary!
     */
    def >(cg : ConditionalGenerator) : Generator = {
      ConditionalTripleGenerator(prop, cg)
    }
    /**
     * Generate an untyped string data property triple.
     * Note this does not generate the same as "foo"^^xsd.string
     */
    def >(value : String) : Generator = DTripleGenerator(prop, FixedTextGenerator(value))
    /**
     * Generate an untyped boolean data property triple
     */
    def >(value : Boolean) : Generator = {
      DTripleGenerator(prop, (FixedTextGenerator(value.toString) ^^ xsd.boolean))
    }
    /**
     * Generate an untyped int data property triple
     */
    def >(value : Int) : Generator = {
      DTripleGenerator(prop, (FixedTextGenerator(value.toString) ^^ xsd.integer))
    }
    /**
     * Generate an untyped decimal data property triple
     */
    def >(value : Double) : Generator = {
      DTripleGenerator(prop, (FixedTextGenerator(value.toString) ^^ xsd.decimal))
    }
    /**
     * Generate an object property triple with the active node as the object and 
     * the value as the subject (i.e., a 'backlink')
     */
    def <(nr : NodeRequest) : Generator = IOTripleGenerator(prop, nr)
    /**
     * Generate an object property triple with the active node as the object and
     * the value as the subject, which is also a new triple block.
     */
    def <(ng : NodeGenerator) : Generator = INTripleGenerator(prop, ng)
    lazy val lookup = name match {
      case FixedTextGenerator(name) => (namespace,name)
      case _ => throw new RuntimeException("Looking up non-fixed namespace")
    }
    override def when(cond : Condition) : Request with Handleable = HandleableConditionRequest(this, cond)
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

  private[lixr] case class ConditionRequest(req : Request, cond : Condition) extends Request

  private[lixr] case class HandleableConditionRequest(req : Handleable with Request, 
    cond : Condition) extends Request with Handleable {
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

  private[lixr] case class OTripleGenerator(prop : TextGenerator, value : NodeRequest) extends Generator 

  private[lixr] case class DTripleGenerator(prop : TextGenerator, value : DataValueGenerator) extends Generator 

  private[lixr] case class NTripleGenerator(prop : TextGenerator, value : NodeGenerator) extends Generator 

  private[lixr] case class IOTripleGenerator(prop : TextGenerator, value : NodeRequest) extends Generator 

  private[lixr] case class INTripleGenerator(prop : TextGenerator, value : NodeGenerator) extends Generator 

  private[lixr] case class ConditionalTripleGenerator(prop : TextGenerator, value : ConditionalGenerator) extends Generator

  private[lixr] case class AttributeGenerator(name : NodeRequest, text : TextGenerator) extends Generator

  private[lixr] case class TextContentGenerator(content : TextGenerator) extends Generator

  private[lixr] case class XMLContentGenerator(content : Elem) extends Generator

  private[lixr] case class RecursiveGenerator(request : Request) extends Generator

  private[lixr] case class FailGenerator(message : TextGenerator) extends Generator

  private[lixr] case class CommentGenerator(message : TextGenerator) extends Generator

  private[lixr] case class MessageGenerator(message : TextGenerator) extends Generator

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
    def ===(target : TextGenerator) : Condition = EqualityCondition(this,target)
    /**
     * Check equality of text generators
     */
    def ===(target : String) : Condition = EqualityCondition(this, FixedTextGenerator(target))
    /**
     * Check equality of text generators
     */
    def !==(target : TextGenerator) : Condition = InequalityCondition(this,target)
    /**
     * Check inequality of text generators
     */
    def !==(target : String) : Condition = InequalityCondition(this, FixedTextGenerator(target))
    /**
     * Check whether text generator matches a fixed regular expression
     */
    def matches(regex : String) : Condition = RegexCondition(this, regex)
    /**
     * Check whether text generator matches a non-empty string
     */
    def exists : Condition = TextExistenceCondition(this)
    /**
     * Concatenate a text generator
     */
    def +:(s : String) : TextGenerator = AppendTextGenerator(Some(s),this,None)
    /**
     * Concatenate a text generator
     */
    def :+(s : String) : TextGenerator = AppendTextGenerator(None, this, Some(s))
    /**
     * Concatenate a text generator
     */
    def +:(s : TextGenerator) : TextGenerator = ConcatTextGenerator(s, this)
    /**
     * Concatenate a text generator
     */
    def :+(s : TextGenerator) : TextGenerator = ConcatTextGenerator(this, s)
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

  private[lixr] case class FragmentGenerator(frag : TextGenerator) extends TextGenerator

  private[lixr] case class SetVariable(name : String, value : TextGenerator, context : Seq[Generator]) extends Generator

  private[lixr] case class GetVariable(name : String) extends TextGenerator

  private[lixr] case class AppendTextGenerator(left : Option[String], generator : TextGenerator, right : Option[String]) extends TextGenerator {
    override def +:(s : String) : TextGenerator = AppendTextGenerator(Some(left.getOrElse("") + s), generator, right)
    override def :+(s : String) : TextGenerator = AppendTextGenerator(left, generator, Some(right.getOrElse("") + s))
  }

  private[lixr] case class ConcatTextGenerator(first : TextGenerator, second : TextGenerator) extends TextGenerator

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

  /**
   * Perform a regular expression search and replace on a string
   * @param tg The text to apply the search and replace to
   * @param regex1 The regex to search for
   * @param regex2 The regex to replace with
   */
  def replace(tg : TextGenerator, regex1 : String, regex2 : String) = 
    transform(tg)(_.replaceAll(regex1, regex2))(_.replaceAll(regex2, regex1))

  /**
   * Take a substring of a string. If the string is shorter than start or end the longest match is returned
   * @param tg The text to take a substring from
   * @param start The start index
   * @param end The end index
   */
  def substring(tg : TextGenerator, start : Int, end : Int) =
    transform(tg)(_.slice(start, end))(throw new UnsupportedOperationException())

  private[lixr] case class ForGenerator(req : Request, body : Seq[Generator]) extends Generator

  private[lixr] case class ConditionalGenerator(condition : Condition, result : Seq[Generator], otherwize : Option[Generator]) extends Generator {
    def or(condition2 : Condition)(result2 : Generator*) : ConditionalGenerator = otherwize match {
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

  /**
   * A conditional statement that resolves to true or false
   */
  trait Condition {
    /**
     * Resolve this condition
     * @param resolve Generate a representation of a text expression
     * @param locate Find instances matching a request
     */
    def check(resolve : TextGenerator => Option[String], 
      locate : Request => Seq[Node]) : Boolean
    /**
     * Boolean and
     */
    def and(c2 : Condition) : Condition = ConjunctionCondition(this, c2)
    /**
     * Boolean or
     */
    def or(c2 : Condition) : Condition = DisjunctionCondition(this, c2)
  }

  /**
   * Boolean not
   */
  def not(c : Condition) : Condition = NegationCondition(c)

  private[lixr] case class EqualityCondition(lhs : TextGenerator, rhs : TextGenerator) extends Condition {
    def check(resolve : TextGenerator => Option[String],
      locate : Request => Seq[Node]) = {
      resolve(lhs) == resolve(rhs)
    }
  }

  private[lixr] case class InequalityCondition(lhs : TextGenerator, rhs : TextGenerator) extends Condition {
    def check(resolve : TextGenerator => Option[String],
      locate : Request => Seq[Node]) = resolve(lhs) != resolve(rhs)
  }

  private[lixr] case class RegexCondition(target : TextGenerator, pattern : String) 
      extends Condition {
    def check(resolve : TextGenerator => Option[String],
      locate : Request => Seq[Node]) = resolve(target) match {
      case Some(s) => s.matches(pattern)
      case None => false
    }
  }

  private[lixr] case class RequestExistenceCondition(target : Request) extends Condition {
    def check(resolve : TextGenerator => Option[String],
      locate : Request => Seq[Node]) = !locate(target).isEmpty
  }

  private[lixr] case class TextExistenceCondition(target : TextGenerator) extends Condition {
    def check(resolve : TextGenerator => Option[String],
      locate : Request => Seq[Node]) = resolve(target) != None
  }

  private[lixr] case class ConjunctionCondition(left : Condition, right : Condition) extends Condition {
    def check(resolve : TextGenerator => Option[String],
        locate : Request => Seq[Node]) = 
      left.check(resolve, locate) && right.check(resolve, locate)
  }

  private[lixr] case class DisjunctionCondition(left : Condition, right : Condition) extends Condition {
    def check(resolve : TextGenerator => Option[String],
        locate : Request => Seq[Node]) = 
      left.check(resolve, locate) || right.check(resolve, locate)
  }

  private[lixr] case class NegationCondition(cond : Condition) extends Condition {
    def check(resolve : TextGenerator => Option[String],
        locate : Request => Seq[Node]) = 
      !cond.check(resolve, locate)
  }

  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : NodeRequest, text : TextGenerator) : Generator = AttributeGenerator(name, text)
  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : NodeRequest, str : String) : Generator = AttributeGenerator(name, FixedTextGenerator(str))
  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : String, text : TextGenerator) : Generator = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),text)
  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : String, str : String) : Generator = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),FixedTextGenerator(str))
  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : Symbol, text : TextGenerator) : Generator = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),text)
  /** Generate an XML attribute with the given value, does not generate any RDF! */
  def xmlAtt(name : Symbol, str : String) : Generator = AttributeGenerator(NodeRequest.resolveStringAsRequest(name),FixedTextGenerator(str))

  /**
   * Generate text content for current XML tag, does not generate any RDF
   */
  def text(value : String) = TextContentGenerator(FixedTextGenerator(value))
  /**
   * Generate text content for current XML tag, does not generate any RDF
   */
  def text(value : TextGenerator) = TextContentGenerator(value)

  /**
   * Call the appropriate handler for each match
   */
  def handle(nodes : Request) = RecursiveGenerator(nodes)
  /**
   * Call the appropriate handler for each match
   */
  def handle(name : Symbol) = RecursiveGenerator(NodeRequest.resolveStringAsRequest(name))

  /**
   * End the conversion with a the given error message
   */
  def fail(message : TextGenerator) = FailGenerator(message)
  /**
   * End the conversion with a the given error message
   */
  def fail(message : String) = FailGenerator(FixedTextGenerator(message))

  /**
   * Output the message as a comment (in the output file)
   */
  def comment(message : TextGenerator) = CommentGenerator(message)
  /**
   * Output the message as a comment (in the output file)
   */
  def comment(message : String) = CommentGenerator(FixedTextGenerator(message))

  /**
   * Output a message to STDERR
   */
  def message(msg : TextGenerator) = MessageGenerator(msg)
  /**
   * Output a message to STDERR
   */
  def message(msg : String) = MessageGenerator(FixedTextGenerator(msg))

  /**
   * Create a new node in the RDF graph
   */
  def node(about : TextGenerator)(triples : Generator*) = NodeGenerator(about, triples)
  /**
   * Create a uri from a string
   */
  def uri(u : String) = NodeRequest(None, FixedTextGenerator(u))
  /**
   * Create a uri from a text generator
   */
  def uri(u : TextGenerator) = NodeRequest(None, u)
  /**
   * Create a new node in the RDF graph
   */
  def node(about : String)(triples : Generator*) = NodeGenerator(FixedTextGenerator(about), triples)

  /**
   * Get the text content of the current node
   */
  def content = ContentGenerator(current)

  /**
   * Get the text content of the requested node
   */
  def content(nr : Request) = ContentGenerator(nr)

  /**
   * Get the XML content of the current node
   */
  def xmlContent = XMLTextGenerator(current)

  /**
   * Get the XML Content of the requested node
   */
  def xmlContent(nr : Request) = XMLTextGenerator(nr)

  /**
   * Generate the following XML without generating any RDF
   */
  def xmlContent(elem : Elem) = XMLContentGenerator(elem)

  /**
   * Generate a random UUID
   */
  def uuid = FixedTextGenerator(java.util.UUID.randomUUID().toString)
  
  /**
   * Generate a unique fragment
   */
  def frag(frags : TextGenerator) = FragmentGenerator(frags)
  /**
   * Generate a unique fragment
   */
  def frag(s : String) = FragmentGenerator(FixedTextGenerator(s))

  /**
   * Create a property from text
   */
  def prop(s : String) = new PropertyPartial(FixedTextGenerator(s))
  /**
   * Create a property from text
   */
  def prop(tg : TextGenerator) = new PropertyPartial(tg)

  /**
   * A property without an object
   */
  class PropertyPartial(tg : TextGenerator) {
    /**
     * Generating an object property triple with the given value
     */
    def >(nr : NodeRequest) : Generator = {
      OTripleGenerator(tg, nr)
    }
    /**
     * Generate a data property triple with the given value
     */
    def >(gen : DataValueGenerator) : Generator = {
      DTripleGenerator(tg, gen)
    }
    /**
     * Generate an object property triple whose value is another node
     */
    def >(ng : NodeGenerator) : Generator = {
      NTripleGenerator(tg, ng)
    }
    /**
     * Conditionally generate an object property
     * @deprecated This supports a syntax that is not necessary!
     */
    def >(cg : ConditionalGenerator) : Generator = {
      ConditionalTripleGenerator(tg, cg)
    }
    /**
     * Generate an untyped string data property triple.
     * Note this does not generate the same as "foo"^^xsd.string
     */
    def >(value : String) : Generator = DTripleGenerator(tg, FixedTextGenerator(value))
    /**
     * Generate an untyped boolean data property triple
     */
    def >(value : Boolean) : Generator = {
      DTripleGenerator(tg, (FixedTextGenerator(value.toString) ^^ xsd.boolean))
    }
    /**
     * Generate an untyped int data property triple
     */
    def >(value : Int) : Generator = {
      DTripleGenerator(tg, (FixedTextGenerator(value.toString) ^^ xsd.integer))
    }
    /**
     * Generate an untyped decimal data property triple
     */
    def >(value : Double) : Generator = {
      DTripleGenerator(tg, (FixedTextGenerator(value.toString) ^^ xsd.decimal))
    }
    /**
     * Generate an object property triple with the active node as the object and 
     * the value as the subject (i.e., a 'backlink')
     */
    def <(nr : NodeRequest) : Generator = IOTripleGenerator(tg, nr)
    /**
     * Generate an object property triple with the active node as the object and
     * the value as the subject, which is also a new triple block.
     */
    def <(ng : NodeGenerator) : Generator = INTripleGenerator(tg, ng)
  }

  def att(name : NodeRequest) = current.att(name)
  def att(s : String) = current.att(s)

  def set(name : String, value : TextGenerator)(context : Generator*) = SetVariable(name, value, context)
  def set(name : String, value : String)(context : Generator*) = SetVariable(name, FixedTextGenerator(value), context)

  def get(name : String) = GetVariable(name)

  def when(condition : Condition)(result : Generator*) = ConditionalGenerator(condition, result, None)

  def forall(req : Request)(body : Generator*) = ForGenerator(req, body)
  def forall(s : String)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s), body)
  def forall(s : Symbol)(body : Generator*) = ForGenerator(NodeRequest.resolveStringAsRequest(s.name), body)

  val rdf_type = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
  val a = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#") + "type"
    
  implicit class StringPimps(s : String) {
    def --> (foo : Generator*) = NodeRequest.resolveStringAsRequest(s).-->(foo:_*)
    def when (condition : Condition) = NodeRequest.resolveStringAsRequest(s).when(condition)
    def ^^(nodeRequest : NodeRequest) = TypedTextGenerator(FixedTextGenerator(s), nodeRequest)
    def ^^(text : TextGenerator) = TypedTextGenerator(FixedTextGenerator(s), NodeRequest(None, text))
    def ^^(text : String) = TypedTextGenerator(FixedTextGenerator(s), NodeRequest(None, FixedTextGenerator(text)))
    def @@(text : TextGenerator) = LangTextGenerator(FixedTextGenerator(s), text)
    def @@(text : String) = LangTextGenerator(FixedTextGenerator(s), FixedTextGenerator(text))
  }
  
  implicit class SymbolPimps(s : Symbol) {
    def --> (foo : Generator*) = NodeRequest.resolveStringAsRequest(s.name).-->(foo:_*)
    def when(cond : Condition) = NodeRequest.resolveStringAsRequest(s.name).when(cond)
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
