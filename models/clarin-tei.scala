import eu.liderproject.lixr.Clarin

new Clarin {
  val ms = Namespace("http://purl.org/ms-lod/MetaShare.ttl#")
  val tei = cmd

  cmd.Components --> handle(cmd.teiHeader)

  cmd.teiHeader --> (
  	handle(cmd.fileDesc)
  )

  cmd.fileDesc --> (
  	handle(cmd.titleStmt)
  )

  cmd.titleStmt --> (
  	tei.titleStmt > node(frag("titleStmt")) (
  	  handle(cmd.title),
  	  handle(cmd.author),
<<<<<<< HEAD:src/main/scala/lixr/models/clarin-tei.scala
  	  handle(cmd.editor),
  	  handle(cmd.respStmt),
  	  handle(cmd.orgName),
=======
  	  handle(cmd.editor)
>>>>>>> fcf13c7044b90793e67ca8e7ba28e3f2045b12f8:models/clarin-tei.scala
  	)
  )

  cmd.title --> (
  	tei.title > node(frag("title")) (
  	  a > tei.TitleStmt,
  	  rdf.value > content,
  	  stringAttMap(current, "type", tei.titleType),
  	  stringAttMap(current, "level", tei.titleLevel)//,
  	  //intAttMap(current, "n", tei.titleN)
  	)
  )

  cmd.author --> (
  	tei.author > node(frag("author")) (
  	  a > foaf.Person,
  	  handle(cmd.persName)
  	)
  )

  cmd.persName --> (
  	stringMap(cmd.surname, foaf.familyName),
  	stringMap(cmd.forename, foaf.givenName),
  	stringMap(cmd.nameLink, tei.nameLink),
  	stringMap(cmd.genName, tei.genName),
  	stringMap(cmd.addName, tei.addName),
  	stringMap(cmd.roleName, tei.roleName),
  	forall(cmd.idno) (
  	  handle(cmd.idno)
  	)
  )

  cmd.idno --> (
  	when(att("type") === "DTAID")(
  	  tei.DTAID > content
  	).or(att("type") === "URN")(
  	  tei.URN > content
	).or(att("type") === "shelfmark")(
	  tei.shelfmark > content
	).or(att("type") === "DTADirName")(
	  tei.DTADirName > content
	).or(att("type") === "URLCatalogue")(
	  tei.URLCatalogue > content
	).or(att("type") === "URLImages")(
	  tei.URLImages > content
	).or(att("type") === "URLWeb")(
	  tei.URLWeb > content
	).or(att("type") === "URLXML")(
	  tei.URLXML > content
	).or(att("type") === "URLText")(
	  tei.URLText > content
	).or(att("type") === "URLHTML")(
	  tei.URLHTML > content
	).or(att("type") === "URLCAB")(
	  tei.URLCAB > content
	).or(att("type") === "URLTCF")(
	  tei.URLTCF > content
	).or(att("type") === "PND")(
	  tei.PND > content
	).or(att("type") === "PIDCMDI")(
	  tei.PIDCMDI > content
	)
  )

  cmd.editor --> (
  	tei.editor > node(frag("editor")) (
  	  a > foaf.Person,
  	  handle(cmd.persName),
  	  stringMap(cmd.role, tei.role)
  	)
  )
<<<<<<< HEAD:src/main/scala/lixr/models/clarin-tei.scala

  cmd.respStmt --> (
  	tei.respStmt > node(frag("respStmt")) (
  	  a > foaf.Person,
  	  handle(cmd.persName)
  	)
  )

  cmd.orgName --> (
  	forall(cmd.orgName)(
  	  when(att("type") === "project") (
  	    tei.project > node(frag("project")) (
  	      rdf.value > content,
  	      handle(parent \ cmd.idno)
  	    )
  	  ).or(att("type") === "hostingInstitution") (
  	    tei.hostingInstitution > node(frag("hostingInstitution")) (
  	      rdf.value > content,
  	      handle(parent \ cmd.idno)
  	  )
  	)
  )
}
=======
}
>>>>>>> fcf13c7044b90793e67ca8e7ba28e3f2045b12f8:models/clarin-tei.scala
