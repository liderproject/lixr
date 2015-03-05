package eu.liderproject.lixr

class ClarinMods extends Clarin {
  val ms = Namespace("http://purl.org/ms-lod/MetaShare.ttl#")
  val mods = cmd

  cmd.Components --> handle(cmd.mods)

  def trinaryMap(tag : NodeRequest, prop : NodeRequest) = {
    forall(tag)(
      when(content === "yes")(
        prop > true
      ).or(content === "no")(
        prop > false
      ).or(content === "unknown")(
        prop > text("unknown")
      )
    )
  }

  cmd.mods --> (
    trinaryMap(cmd.edboOverlap, mods.edboOverlap),
    trinaryMap(cmd.machineReadable, mods.machineReadable),
    objectMap(cmd.typeOfResource, mods.typeOfResource,
      "text" -> mods.text,
      "moving image" -> mods.movingImage,
      "collection / mixed material" -> mods.collectionOrMixedMaterial,
      "notated music" -> mods.notatedMusic,
      "cartographic" -> mods.cartographics,
      "sound recording-musical" -> mods.soundRecordingMusical,
      "sound recording-nonmusical" -> mods.soundRecordingNonmusical,
      "sound recording" -> mods.soundRecording,
      "still image" -> mods.stillImage,
      "sound recording-musical" -> mods.soundRecordingMusical,
      "three dimensional object" -> mods.threeDimensionalObject,
      "software, multimedia" -> mods.softwareOrMultimedia,
      "mixed material" -> mods.mixedMaterial),
    linkMap(cmd.genre, mods.genre),
    langStringMap(cmd.`abstract`, mods.`abstract`),
    langStringMap(cmd.targetAudience, mods.targetAudience),
    langStringMap(cmd.note, mods.note),
    langStringMap(cmd.classification, mods.classification),
    stringMap(cmd.identifier, dc.identifier),
    handle(cmd.titleInfo),
    handle(cmd.name),
    handle(cmd.originInfo),
    handle(cmd.language),
    handle(cmd.physicalDescription),
    handle(cmd.subject),
    handle(cmd.relatedItem),
    handle(cmd.location),
    handle(cmd.accessCondition),
    handle(cmd.part),
    handle(cmd.extension),
    handle(cmd.recordInfo)
  )

  cmd.titleInfo --> (
    langStringMap(cmd.title, dc.title),
    langStringMap(cmd.subTitle, mods.subTitle),
    intMap(cmd.partNumber, mods.partNumber),
    langStringMap(cmd.partName, mods.partName),
    stringMap(cmd.nonSort, mods.nonSort), // No clue what this property means and it is never used!
    stringAttMap(current, "type", dc.`type`),
    stringAttMap(current, "authority", mods.titleAuthority),
    stringAttMap(current, "authorityURI", mods.titleAuthorityURI),
    stringAttMap(current, "valueURI", mods.titleValueURI),
    stringAttMap(current, "displayLabel", rdfs.label),
    stringAttMap(current, "supplied", mods.titleSupplied),
    stringAttMap(current, "usage", mods.titleUsage),
    stringAttMap(current, "altRepGroup", mods.titleAltRepGroup),
    stringAttMap(current, "nameTitleGroup", mods.titleNameTitleGroup)
  )

  cmd.name --> (
    mods.name > node(att("ID") or frag("Name"))(
      a > mods.Name,
      forall(cmd.namePart)(
        when(att("type") === "family")(
          foaf.familyName > content
        ).or(att("type") === "given")(
          foaf.givenName > content
        ).or(att("type") === "termsOfAddress")(
          mods.termsOfAddress > content
        )
      )
    ),
    langStringMap(cmd.displayForm, mods.displayForm),
    langStringMap(cmd.affiliation, mods.affiliation),
    handle(cmd.role),
    langStringMap(cmd.description, dc.description),
    stringAttMap(current, "type", dc.`type`),
    stringAttMap(current, "authority", mods.titleAuthority),
    stringAttMap(current, "authorityURI", mods.titleAuthorityURI),
    stringAttMap(current, "valueURI", mods.titleValueURI),
    stringAttMap(current, "displayLabel", rdfs.label),
    stringAttMap(current, "supplied", mods.titleSupplied),
    stringAttMap(current, "usage", mods.titleUsage),
    stringAttMap(current, "altRepGroup", mods.titleAltRepGroup),
    stringAttMap(current, "nameTitleGroup", mods.titleNameTitleGroup)
  )

  cmd.originInfo --> (
    mods.originInfo > node(att("ID") or frag("Origin"))(
      a > mods.OriginInfo,
      langStringMap(cmd.publisher, mods.publisher),
      dateMap(cmd.dateIssued, dct.issued),
      dateMap(cmd.dateCreated, dct.created),
      dateMap(cmd.dateCaptured, mods.dateCaptured),
      dateMap(cmd.dateValid, mods.dateValid),
      dateMap(cmd.dateModified, dct.modified),
      dateMap(cmd.copyrightDate, mods.copyrightDate),
      dateMap(cmd.dateOther, mods.dateOther),
      langStringMap(cmd.edition, mods.edition),
      langStringMap(cmd.issuance, mods.issuance),
      langStringMap(cmd.frequency, mods.frequency),
      forall(cmd.place)(
        langStringMap(cmd.placeTerm, mods.place)
      ),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.language --> (
    ms.languageInfo > node(frag("LanguageInfo")) (
      a > ms.LanguageInfo,
      langStringMap(cmd.languageTerm, ms.languageName),
      langStringMap(cmd.script, ms.languageScript)
    ),
    stringAttMap(current, "objectPart", mods.objectPart),
    stringAttMap(current, "displayLabel", rdfs.label),
    stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
  )

  cmd.role --> ()
}
