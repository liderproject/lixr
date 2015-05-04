import eu.liderproject.lixr._
import java.io.File
import java.net.URI

new Model {
  val resourceURI = System.getProperty("tbx.resourceURI")
  val mapping = new File(Option(System.getProperty("tbx.mappings")).getOrElse("mappings.default"))
  val dcat = Namespace("http://www.w3.org/ns/dcat#")
  val dct = Namespace("http://purl.org/dc/terms/")
  val ontolex = Namespace("http://www.w3.org/ns/lemon/ontolex#")
  val provo = Namespace("http://www.w3.org/ns/prov#")
  val rdfs = Namespace("http://www.w3.org/2000/01/rdf-schema#")
  val skos = Namespace("http://www.w3.org/2004/02/skos/core#")
  val tbx = Namespace("http://tbx2rdf.lider-project.eu/tbx#")

  'martif --> (
    node(resourceURI) (
      rdf_type > dcat.Dataset,
      dct.`type` > att("type"),
      handle('martifHeader),
      handle('text)
    )
  )

  'martifHeader --> (
    rdf_type > tbx.MartifHeader,
    handle('fileDesc),
    handle('encodingDesc),
    handle('revisionDesc)
  )

  'fileDesc --> (
    handle('titleStmt),
    handle('publicationStmt),
    handle('sourceDesc)
  )

  'titleStmt --> (
    langStringMap(content, dct.title),
    handle('note)
  )

  'publicationStmt --> (
    tbx.publicationStmt > xmlContent
  )

  'sourceDesc --> (
    tbx.sourceDesc > xmlContent
  )

  'encodingDesc --> (
    tbx.encodingDesc > xmlContent
  )

  'revisionDesc --> (
    tbx.revisionDesc > xmlContent
  )

  'text --> (
    handle('body),
    handle('back)
  )

  'body --> (
    handle('termEntry)
  )

  'back --> fail("Not yet supported")

  'termEntry --> (
    node(att("id") or ("Term" +: uuid)) (
      rdf_type > skos.Concept,
      handle('langSet),
      auxInfo
    )
  )
  'langSet --> (
    set("lang", att("lang")) (
      handle('ntig),
      handle('tig),
      auxInfo
    )
  )

  'tig --> (
    ontolex.reference < node(frag("sense")) (
      ontolex.sense < node(att("id") or ("Tig-" +: uuid)) (
        handle('term),
        handle('termNote),
        auxInfo,
        ontolex.entry < node("Lexicon" +: get("lang")) (
          rdf_type > ontolex.Lexicon
        )
      )
    )
  )

  'term --> (
    ontolex.canonicalForm > node(frag("canonicalForm")) (
      ontolex.writtenRep > content
    )
  )

  'ntig --> (
    ontolex.reference < node(frag("sense")) (
      ontolex.sense < node(att("id") or ("NTig-" +: uuid)) (
        handle('termGrp),
        auxInfo,
        ontolex.entry < node("Lexicon" +: get("lang")) (
          rdf_type > ontolex.Lexicon
        )
      )
    )
  )

  'descripGrp --> (
    tbx.description > node(att("id") or ("Description-" +: uuid)) (
      rdf_type > tbx.Descrip,
      handle('descrip),
      handle('descripNote),
      handle('admin),
      handle('adminGrp),
      handle('transacGrp),
      handle('note),
      handle('ref),
      handle('xref)
    )
  )

  'adminGrp --> (
    tbx.admin > node(att("id") or ("Admin-" +: uuid)) (
      rdf_type > tbx.Admin,
      handle('admin),
      handle('adminNote),
      handle('note),
      handle('ref),
      handle('xref)
    )
  )

  'transacGrp --> (
    tbx.transaction > node(att("id") or ("Transaction-" +: uuid)) (
      rdf_type > tbx.Transaction,
      rdf_type > provo.Activity,
      handle('transac),
      handle('transacNote),
      handle('date),
      handle('note),
      handle('xref),
      handle('ref)
    )
  )

  'termGrp --> (
    handle('term),
    handle('termNote),
    handle('termNoteGrp),
    handle('termCompList)
  )

  'date --> (
    provo.endedAtTime > (content ^^ xsd.dateTime)
  )

  'termNoteGrp --> (
    tbx.termNote > node(att("id") or ("TermNote-" +: uuid)) (
      rdf_type > tbx.TermNote,
      handle('termNote),
      handle('admin),
      handle('adminGrp),
      handle('transacGrp),
      handle('note),
      handle('ref),
      handle('xref)
    )
  )

  'termCompList --> (
    ontolex.identifies < node(att("id") or ("TermCompList-" +: uuid)) (
      forall('termComp) (
        ontolex.constituent > node(frag("TermComponent")) (
          handle(current)
        )
      ),
      handle('termComp),
      handle('termCompGrp),
      handle('admin),
      handle('adminGrp),
      handle('transacGrp),
      handle('note),
      handle('ref),
      handle('xref)
    )
  )

  'termComp --> (
    ontolex.identifies > node(att("id") or ("TermComp-" +: uuid)) (
      rdf_type > ontolex.LexicalEntry,
      rdfs.label > (content @@ get("lang"))
    )
  )

  'termCompGrp --> (
    ontolex.constituent > node(att("id") or ("TermComp-" +: uuid)) (
      handle('termComp),
      handle('termNote),
      handle('termNoteGrp),
      handle('admin),
      handle('adminGrp),
      handle('transacGrp),
      handle('note),
      handle('ref),
      handle('xref)
    )
  )

  def auxInfo = (
    handle('descrip) ++
    handle('descripGrp) ++
    handle('admin) ++
    handle('adminGrp) ++
    handle('transacGrp) ++
    handle('note) ++
    handle('ref) ++
    handle('xref)
  )

  def langStringMap(gen : PlainTextGenerator, rdf : NodeRequest) = {
    when(att("lang").exists) (
      rdf > (gen @@ att("lang"))
    ) otherwise (
      rdf > gen
    )
  }

  // Read mappings
  val indivMappingLine = "^(\\S*?)\\s*<(\\S*?)>$".r
  val objPropMappingLine = "^(\\S*?)\\s*(\\S*?)\\s*(\\S*?)\\s*<(\\S*)>\\s*OP(\\s*\\{(.*?)\\})?$".r
  val dataPropMappingLine = "^(\\S*?)\\s*(\\S*?)\\s*(\\S*?)\\s*<(\\S*)>\\s*DP(\\s*<(.*?)>)?$".r
  
  val indivs = (io.Source.fromFile(mapping).getLines.flatMap {
    case indivMappingLine(name, uri) => Some(name -> uri)
    case _ => None
  }).toMap

  for(line <- io.Source.fromFile(mapping).getLines) {
    line match {
      case indivMappingLine(_,_) => {}
      case objPropMappingLine(tag, attName, value, puri, null, null) => 
        tag.when((att(attName) === value) and not(att("target").exists)) --> (
          prop(puri) > uri(content)
        )
        tag.when((att(attName) === value) and att("target").exists) --> (
          prop(puri) > uri(att("target"))
        )
      case objPropMappingLine(tag, attName, value, puri, _, alloweds) =>
        for(allowed <- alloweds.split(",")) {
          tag.when((att(attName) === value) and (content === allowed.trim())) --> (
            indivs.get(allowed) match {
              case Some(o) =>
                prop(puri) > uri(o)
              case None =>
                System.err.println("Individual value not found:" + allowed)
                comment("Individual value not found")
            }
          )
        }
      case dataPropMappingLine(tag, attName, value, puri, null, null) =>
        tag.when((att(attName) === value) and not(att("datatype").exists)) --> (
          prop(puri) > content
        )
        tag.when((att(attName) === value) and att("datatype").exists) --> (
          prop(puri) > (content ^^ uri(att("datatype")))
        )
      case dataPropMappingLine(tag, attName, value, puri, _, dturi) =>
        tag.when(att(attName) === value) --> (
          prop(puri) > (content ^^ uri(att("datatype")))
        )
      case line =>
        System.err.println("Bad line:" + line)
    }
  }
}


