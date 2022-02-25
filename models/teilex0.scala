import eu.liderproject.lixr._
import scala.language.dynamics

new ModelWithMappings {
  val tei = Namespace("http://www.tei-c.org/ns/1.0#")

  tei.TEI --> (
    node(att("id") or "#")(
        rendition,
        linking,
        responsibility,
        source,
        typed,   
        when(att("version").exists)(
            tei.version > att("version")),
        handle(tei.teiHeader),
        handle(tei.text)))

  tei.teiHeader --> (
    tei.teiHeader > node(att("id") or ("#" +: uuid))(
        rendition,
        linking,
        responsibility,
        source,
        typed,
        handle(tei.encodingDesc),
        handle(tei.fileDesc)
    ))

  tei.encodingDesc --> (
    tei.encodingDesc > node(att("id") or ("#" +: uuid))(
        rendition,
        linking,
        responsibility,
        source,
        handle(tei.charDecl),
        handle(tei.projectDesc)))

  tei.charDecl --> (
    tei.charDecl > node(att("id") or ("#" +: uuid))(
        rendition,
        linking,
        responsibility,
        source,
        handle(tei.char),
        handle(tei.glyph)))

  tei.char --> (
    tei.char > node(att("id") or ("#" +: uuid))(
        rendition,
        linking,
        responsibility,
        source,
        handle(tei.figure),
        handle(tei.charName),
        handle(tei.charProp),
        handle(tei.mapping)))

  tei.figure --> ()

  tei.text --> ()

  def rendition : Generator = 
    when(att("style").exists)(tei.style > att("style")) ++
    when(att("rendition").exists)(tei.rendition > att("rendition"))

  def linking : Generator = 
    when(att("corresp").exists)(tei.corresp > att("corresp")) ++
    when(att("synch").exists)(tei.synch > att("synch")) ++
    when(att("sameAs").exists)(tei.sameAs > att("sameAs")) ++
    when(att("copyOf").exists)(tei.copyOf > att("copyOf")) ++
    when(att("next").exists)(tei.next > att("next")) ++
    when(att("prev").exists)(tei.prev > att("prev")) ++
    when(att("exclude").exists)(tei.exclude > att("exclude")) ++
    when(att("select").exists)(tei.select > att("select")) 

  def responsibility : Generator = 
    when(att("cert").exists)(tei.cert > att("cert")) ++
    when(att("resp").exists)(tei.resp > att("resp")) 

  def source : Generator = 
    when(att("source").exists)(tei.source > att("source")) 

  def typed : Generator = 
    when(att("type").exists)(tei.`type` > att("type")) ++
    when(att("subtype").exists)(tei.subtype > att("subtype")) 
}
