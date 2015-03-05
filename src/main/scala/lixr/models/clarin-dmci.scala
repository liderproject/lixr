package eu.liderproject.lixr

object ClarinDmci extends Clarin {
  cmd.Components --> (
    handle(cmd.`OLAC-DcmiTerms`),
    handle(cmd.DcmiTerms)
  )

  cmd.`OLAC-DcmiTerms` --> dcmiTerms
  cmd.DcmiTerms --> dcmiTerms
  def dcmiTerms = when(content(cmd.`abstract`) !== "") (
      dct.`abstract` > content(cmd.`abstract`)
    ).or(content(cmd.accessRights) !== "") (
      dct.accessRights > content(cmd.accessRights)
    ).or(content(cmd.accrualMethod) !== "") (
      dct.accrualMethod > content(cmd.accrualMethod)
    ).or(content(cmd.accrualPeriodicity) !== "") (
      dct.accrualPeriodicity > content(cmd.accrualPeriodicity)
    ).or(content(cmd.accrualPolicy) !== "") (
      dct.accrualPolicy > content(cmd.accrualPolicy)
    ).or(content(cmd.alternative) !== "") (
      dct.alternative > content(cmd.alternative)
    ).or(content(cmd.audience) !== "") (
      dct.audience > content(cmd.audience)
    ).or(content(cmd.available) !== "") (
      dct.available > content(cmd.available)
    ).or(content(cmd.bibliographicCitation) !== "") (
      dct.bibliographicCitation > content(cmd.bibliographicCitation)
    ).or(content(cmd.conformsTo) !== "") (
      dct.conformsTo > content(cmd.conformsTo)
    ).or(content(cmd.contributor) !== "") (
      dc.contributor > content(cmd.contributor)
    ).or(content(cmd.coverage) !== "") (
      dc.coverage > content(cmd.coverage)
    ).or(content(cmd.created) !== "") (
      dct.created > content(cmd.created)
    ).or(content(cmd.creator) !== "") (
      dc.creator > content(cmd.creator)
    ).or(content(cmd.date) !== "") (
      dc.date > content(cmd.date)
    ).or(content(cmd.dateAccepted) !== "") (
      dct.dateAccepted > content(cmd.dateAccepted)
    ).or(content(cmd.dateSubmitted) !== "") (
      dct.dateSubmitted > content(cmd.dateSubmitted)
    ).or(content(cmd.description) !== "") (
      dc.description > content(cmd.description)
    ).or(content(cmd.educationLevel) !== "") (
      dct.educationLevel > content(cmd.educationLevel)
    ).or(content(cmd.extent) !== "") (
      dct.extent > content(cmd.extent)
    ).or(content(cmd.format) !== "") (
      dc.format > content(cmd.format)
    ).or(content(cmd.hasFormat) !== "") (
      dct.hasFormat > content(cmd.hasFormat)
    ).or(content(cmd.hasPart) !== "") (
      dct.hasPart > content(cmd.hasPart)
    ).or(content(cmd.hasVersion) !== "") (
      dct.hasVersion > content(cmd.hasVersion)
    ).or(content(cmd.relation) !== "") (
      dc.relation > content(cmd.relation)
    ).or(content(cmd.identifier) !== "") (
      dc.identifier > content(cmd.identifier)
    ).or(content(cmd.instructionalMethod) !== "") (
      dct.instructionalMethod > content(cmd.instructionalMethod)
    ).or(content(cmd.isFormatOf) !== "") (
      dct.isFormatOf > content(cmd.isFormatOf)
    ).or(content(cmd.isPartOf) !== "") (
      dct.isPartOf > content(cmd.isPartOf)
    ).or(content(cmd.isReferencedBy) !== "") (
      dct.isReferencedBy > content(cmd.isReferencedBy)
    ).or(content(cmd.isRequiredBy) !== "") (
      dct.isRequiredBy > content(cmd.isRequiredBy)
    ).or(content(cmd.isReplacedBy) !== "") (
      dct.isisReplacedBy > content(cmd.isReplacedBy)
    ).or(content(cmd.issued) !== "") (
      dct.issued > content(cmd.issued)
    ).or(content(cmd.isVersionOf) !== "") (
      dct.isVersionOf > content(cmd.isVersionOf)
    ).or(content(cmd.language) !== "") (
      dc.language > content(cmd.language)
    ).or(content(cmd.license) !== "") (
      dct.license > content(cmd.license)
    ).or(content(cmd.mediator) !== "") (
      dct.mediator > content(cmd.mediator)
    ).or(content(cmd.medium) !== "") (
      dct.medium > content(cmd.medium)
    ).or(content(cmd.modified) !== "") (
      dct.modified > content(cmd.modified)
    ).or(content(cmd.provenance) !== "") (
      dct.provenance > content(cmd.provenance)
    ).or(content(cmd.publisher) !== "") (
      dc.publisher > content(cmd.publisher)
    ).or(content(cmd.references) !== "") (
      dct.references > content(cmd.references)
    ).or(content(cmd.relation) !== "") (
      dct.relation > content(cmd.relation)
    ).or(content(cmd.replaces) !== "") (
      dct.replaces > content(cmd.replaces)
    ).or(content(cmd.requires) !== "") (
      dct.requires > content(cmd.requires)
    ).or(content(cmd.rights) !== "") (
      dc.rights > content(cmd.rights)
    ).or(content(cmd.rightsHolder) !== "") (
      dct.rightsHolder > content(cmd.rightsHolder)
    ).or(content(cmd.source) !== "") (
      dc.source > content(cmd.source)
    ).or(content(cmd.spatial) !== "") (
      dct.spatial > content(cmd.spatial)
    ).or(content(cmd.subject) !== "") (
      dc.subject > content(cmd.subject)
    ).or(content(cmd.tableOfContents) !== "") (
      dct.tableOfContents > content(cmd.tableOfContents)
    ).or(content(cmd.temporal) !== "") (
      dct.temporal > content(cmd.temporal)
    ).or(content(cmd.title) !== "") (
      dc.title > content(cmd.title)
    ).or(content(cmd.`type`) !== "") (
      dc.`type` > content(cmd.`type`)
    ).or(content(cmd.valid) !== "") (
      dct.valid > content(cmd.valid)
    )

}
