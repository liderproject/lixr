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
      "cartographic" -> mods.cartographic,
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

  cmd.role --> (
    forall(cmd.role)(
      langStringMap(cmd.roleTerm, mods.role)
    )
  )

  cmd.originInfo --> ( // Maybe CaptureInfo
    mods.originInfo > node(frag("originInfo")) (
      langStringMap(cmd.publisher, dc.publisher),
      dateMap(cmd.dateIssued, dc.issued),
      dateMap(cmd.dateCreated, dc.created),
      dateMap(cmd.dateCaptured, mods.dateCaptured),
      dateMap(cmd.dateModified, mods.dateModified),
      dateMap(cmd.dateValid, mods.dateValid),
      dateMap(cmd.copyrightDate, mods.copyrightDate),
      dateMap(cmd.dateOther, mods.dateOther),
      langStringMap(cmd.edition, mods.edition),
      stringMap(cmd.issuance, mods.issuance),
      langStringMap(cmd.frequency, mods.frequency),
      forall(cmd.place)(
        langStringMap(cmd.placeTerm, mods.place)
      ),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.language --> ( // Maybe LanguageInfo
    mods.language > node(frag("language")) (
      langStringMap(cmd.languageTerm, mods.languageTerm),
      langStringMap(cmd.scriptTerm, mods.scriptTerm),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.physicalDescription --> ( // Maybe MediaInfo
    mods.physicalDescription > node(frag("physicalDescription")) (
      langStringMap(cmd.form, mods.form),
      stringMap(cmd.reformattingQuality, mods.reformattingQuality),
      langStringMap(cmd.internetMedia, mods.internetMedia), // mime type??
      langStringMap(cmd.extent, mods.extent),
      stringMap(cmd.digitalOrigin, mods.digitalOrigin),
      langStringMap(cmd.note, rdfs.comment),      
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.subject --> (
    mods.subject > node(frag("subject")) (
      langStringMap(cmd.topic, dc.subject), // I think
      langStringMap(cmd.geographic, mods.geographic), // Is there a DC prop
      langStringMap(cmd.temporal, dct.temporal),
      handle(cmd.titleInfo),
      handle(cmd.name),
      langStringMap(cmd.geographicCode, mods.geographicCode),
      langStringMap(cmd.genre, mods.genre),
      handle(cmd.hierarchicalGeographic),
      handle(cmd.cartographics),
      stringAttMap(current, "authority", mods.titleAuthority),
      stringAttMap(current, "authorityURI", mods.titleAuthorityURI),
      stringAttMap(current, "valueURI", mods.titleValueURI),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "supplied", mods.titleSupplied),
      stringAttMap(current, "usage", mods.titleUsage),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)

    )
  )

  cmd.titleInfo --> (
    langStringMap(cmd.title, dc.title),
    mods.titleInfo > node(frag("titleInfo")) (
      langStringMap(cmd.title, mods.title),
      langStringMap(cmd.subTitle, mods.subTitle),
      langStringMap(cmd.partNumber, mods.partNumber),
      langStringMap(cmd.partName, mods.partName),
      langStringMap(cmd.nonSort, mods.nonSort),
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
  )

  cmd.name --> (
    mods.name > node(frag("name")) (
      langStringMap(cmd.namePart, mods.namePart),
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
  )

  cmd.hierarchicalGeographic --> (
    mods.hierarchicalGeographic > node(frag("hierarchicalGeographic")) (
      langStringMap(cmd.continent, mods.continent),
      langStringMap(cmd.country, mods.country),
      langStringMap(cmd.province, mods.province),
      langStringMap(cmd.region, mods.region),
      langStringMap(cmd.state, mods.state),
      langStringMap(cmd.territory, mods.territory),
      langStringMap(cmd.county, mods.county),
      langStringMap(cmd.city, mods.city),
      langStringMap(cmd.islang, mods.island),
      langStringMap(cmd.area, mods.area),
      langStringMap(cmd.extraterrestrialArea, mods.extraterrestrialArea),
      langStringMap(cmd.citySection, mods.citySection)
    )
  )

  cmd.cartographics --> (
    mods.cartographics > node(frag("cartographics")) (
      langStringMap(cmd.scale, mods.scale),
      langStringMap(cmd.projection, mods.projection),
      langStringMap(cmd.coordinates, mods.coordinates),
      langStringMap(cmd.occupation, mods.occupation)
    )
  )

  cmd.relatedItem --> (
    mods.relatedItem > node(frag("relatedItem")) (
      objectMap(cmd.typeOfResource, mods.typeOfResource,
        "text" -> mods.text,
        "moving image" -> mods.movingImage,
        "collection / mixed material" -> mods.collectionOrMixedMaterial,
        "notated music" -> mods.notatedMusic,
        "cartographic" -> mods.cartographic,
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
      langStringMap(cmd.identifier, dc.identifier),
      handle(cmd.titleInfo),
      handle(cmd.name),
      handle(cmd.originInfo),
      handle(cmd.language),
      handle(cmd.physicalDescription),
      handle(cmd.subject),
      handle(cmd.location),
      langStringMap(cmd.accessCondition, mods.accessCondition),
      handle(cmd.part),
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
  )

  cmd.location --> (
    mods.location > node(frag("location")) (
      langStringMap(cmd.physicalLocation, mods.physicalLocation),
      langStringMap(cmd.shelfLocator, mods.shelfLocator),
      linkMap(cmd.url, mods.url),
      handle(cmd.holdingSimple),
      stringMap(cmd.holdingExternal, mods.holdingExternal), // Double check
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.holdingSimple --> (
    handle(cmd.copyInformation)
  )

  cmd.copyInformation --> (
    mods.holdingSimple > node(frag("copyInformation")) (
      langStringMap(cmd.form, mods.form),
      langStringMap(cmd.subLocation, mods.subLocation),
      langStringMap(cmd.shelfLocator, mods.shelfLocator),
      langStringMap(cmd.electronicLocator, mods.electronicLocator),
      langStringMap(cmd.note, rdfs.comment),
      langStringMap(cmd.enumerationAndChronology, mods.enumerationAndChronology)
    )
  )

  cmd.part --> (
    mods.part > node(frag("part")) (
      handle(cmd.detail),
      handle(cmd.extent),
      dateMap(cmd.date, dc.date),
      langStringMap(cmd.text, mods.text),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.detail --> (
    mods.detail > node(frag("detail")) (
      langStringMap(cmd.number, mods.number),
      langStringMap(cmd.caption, mods.caption),
      langStringMap(cmd.title, dc.title)
    )
  )

  cmd.extent --> (
    mods.extent > node(frag("extent")) (
      langStringMap(cmd.start, mods.start),
      langStringMap(cmd.end, mods.end),
      stringMap(cmd.total, mods.total),
      langStringMap(cmd.list, mods.list)
    )
  )

  cmd.extension --> (
    mods.extension > node(frag("extension")) (
      stringAttMap(current, "displayLabel", rdfs.label)
    )
  )

  cmd.recordInfo --> ( // RecordInfo?
    mods.recordInfo > node(frag("recordInfo")) (
      langStringMap(cmd.recordContentSource, dc.source),
      dateMap(cmd.recordCreationDate, dc.created),
      dateMap(cmd.recordChangeDate, dc.modified),
      langStringMap(cmd.recordIdentifier, dc.identifier),
      langStringMap(cmd.recordOrigin, mods.recordOrigin),
      handle(cmd.languageOfCataloging),
      langStringMap(cmd.descriptionStandard, mods.descriptionStandard),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

  cmd.languageOfCataloging --> ( // Maybe LanguageInfo
    mods.language > node(frag("language")) (
      langStringMap(cmd.languageTerm, mods.languageTerm),
      langStringMap(cmd.scriptTerm, mods.scriptTerm),
      stringMap(cmd.total, mods.total),
      langStringMap(cmd.list, mods.list),
      stringAttMap(current, "displayLabel", rdfs.label),
      stringAttMap(current, "altRepGroup", mods.titleAltRepGroup)
    )
  )

}
