package eu.liderproject.lixr

class Clarin extends eu.liderproject.lixr.ModelWithMappings {
  val cmd = Namespace("http://www.clarin.eu/cmd/")
  val cmdi = Namespace("http://www.clarin.eu/cmd/")
  val dc = Namespace("http://purl.org/dc/elements/1.1/")
  val dcat = Namespace("http://www.w3.org/ns/dcat#")
  val dct = Namespace("http://purl.org/dc/terms/")
  val foaf = Namespace("http://xmlns.com/foaf/0.1/")
  val rdf = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  val xsl = Namespace("http://www.w3.org/1999/XSL/Transform")

  cmd.CMD --> (
    node("Header") (
      a > dcat.CatalogRecord,
      handle(cmd.Header),
      foaf.primaryTopic > node("") (
        handle(cmd.Resources),
        handle(cmd.Components)
      )
    )
  )

  cmd.Header --> (
    handle(cmd.MdCreator),
    handle(cmd.MdCreationDate),
    handle(cmd.MdSelfLink),
    handle(cmd.MdProfile),
    handle(cmd.MdCollectionDisplayName)
  )

  cmd.MdCreator --> (
    dc.creator > content
  )

  (cmd.MdCreationDate when (content !== "")) --> (
    dct.issued > (content ^^ xsd.date)
  )

  cmd.MdSelfLink --> (
    cmd.MdSelfLink > content
  )

  cmd.MdProfile --> (
    cmd.MdProfile > content
  )

  cmd.MdCollectionDisplayName --> (
    dc.title > content
  )

  cmd.Resources --> (
    handle(cmd.ResourceProxyList),
    handle(cmd.JournalFileProxyList),
    handle(cmd.ResourceRelationList),
    handle(cmd.IsPartOfList)
  )

  cmd.ResourceProxyList --> (
    handle(cmd.ResourceProxy)
  )

  cmd.ResourceProxy --> (
    dcat.distribution > node(att("id")) (
      handle(cmd.ResourceType),
      handle(cmd.ResourceRef)
    )
  )

  cmd.ResourceType --> (
    cmd.ResourceType > content
  )

  cmd.ResourceRef --> (
    resolveUri(cmd.ResourceRef)
  )

  (cmd.JournalFileProxyList when (not(current.isEmpty))) --> (
    comment(content)
  )

  (cmd.ResourceRelationList when (not(current.isEmpty))) --> (
    comment(content)
  )

  cmd.IsPartOfList --> (
    handle(cmd.IsPartOf)
  )

  cmd.IsPartOf --> (
    resolveUri(cmd.IsPartOf)
  )

    
//  def resolveUri = when(content matches ("^hdl:")) (
//    uri("http://hdl.handle.net" +: (content.substring(0, 4)))
//  ) otherwise (
//    uri(content)
//  )

  def resolveUri(prop : NodeRequest) = when(content matches ("^hdl:")) (
    prop > node("http://hdl.handle.net" +: (content.substring(0, 4)))()
  ) otherwise (
    prop > node(content)()
  )

}



