import eu.liderproject.lixr.Clarin

new Clarin {
  val ms = Namespace("http://purl.org/ms-lod/MetaShare.ttl#")
  val imdi = cmd

  override def lang = "LanguageId"

  cmd.Component --> (handle(cmd.Session))

  cmd.Session --> (
    stringMap(cmd.Name, rdfs.label),
    stringMap(cmd.Title, dc.title),
    dateMap(cmd.Date, dc.date),
    handle(cmd.descriptions),
    handle(cmd.MDGroup),
    handle(cmd.Resources),
    handle(cmd.References)
  )

  cmd.descriptions --> (
    langStringMap(cmd.Description, dc.description)
  )

  cmd.MDGroup --> (
    handle(cmd.Location),
    handle(cmd.Project),
    handle(cmd.Keys),
    handle(cmd.Content),
    handle(cmd.Actors)
  )

  cmd.Location --> (
    objectMap(cmd.Continent, imdi.continent,
      "Unknown" -> imdi.UnknownContinent,
//      "Unspecified"
      "Africa" -> imdi.Africa,
      "Asia" -> imdi.Asia,
      "Europe" -> imdi.Europe,
      "Australia" -> imdi.Australia,
      "Oceania" -> imdi.Oceania,
      "North-America" -> imdi.NorthAmerica,
      "Middle-America" -> imdi.MiddleAmerica,
      "South-America" -> imdi.SouthAmerica),
    stringMap(cmd.Country, ms.country),
    stringMap(cmd.Region, ms.region),
    stringMap(cmd.Address, ms.address)
  )

  cmd.Project --> (
    imdi.project > node(frag("Project")) (
      stringMap(cmd.Name, rdfs.label),
      stringMap(cmd.Title, dc.title),
      stringMap(cmd.Id, dc.identifier),
      handle(cmd.Contact),
      handle(cmd.descriptions)
    )
  )

  cmd.Contact --> (
    dcat.contactPoint > node(frag("Contact"))(
      a > foaf.Person,
      stringMap(cmd.Name, foaf.name),
      stringMap(cmd.Address, ms.address),
      stringMap(cmd.Email, ms.email),
      stringMap(cmd.Organization, imdi.organization)
    )
  )

  cmd.Keys --> (
    stringAttMap(cmd.Key, "Name", imdi.key)
  )

  cmd.Content --> (
    imdi.content > node(frag("Content")) (
      stringMap(cmd.Genre, imdi.genre),
      stringMap(cmd.SubGenre, imdi.subGenre),
      stringMap(cmd.Task, imdi.task),
      stringMap(cmd.Modalities, imdi.modalities),
      stringMap(cmd.Subject, imdi.subject),
      handle(cmd.CommunicationContext),
      handle(cmd.Content_Languages),
      handle(cmd.Keys),
      handle(cmd.descriptions)
    )
  )

  cmd.CommunicationContext --> (
    objectMap(cmd.Interactivity, ms.interactivity,
      "Unknown" -> ms.other,
      "interactive" -> ms.interactive,
      "non-interactive" -> ms.nonInteractive,
      "semi-interactive" -> ms.semiInteractive),
    objectMap(cmd.PlanningType, ms.naturality,
      "Unknown" -> ms.other,
      "spontaneous" -> ms.spontaneous,
      "semi-spontaneous" -> ms.semiPlanned,
      "planned" -> ms.planned
      ),
    objectMap(cmd.Involvement, imdi.involvement,
      "Unknown" -> imdi.unknownInvolvement,
      "elicited" -> imdi.elicited,
      "non-elicited" -> imdi.nonElicited,
      "no-observer" -> imdi.noObserver      
      ),
    objectMap(cmd.SocialContext, imdi.socialContext,
      "Unknown" -> imdi.unknownSocialContext,
      "Family" -> imdi.familySocialContext,
      "Private" -> imdi.privateSocialContext,
      "Public" -> imdi.publicSocialContext,
      "Controlled environment" -> imdi.controlledEnvironmentSocialContext),
    objectMap(cmd.EventStructure, ms.conversationalType,
      "Unknown" -> ms.unknownConversationalType,
      "Monologue" -> ms.monologue,
      "Dialogue" -> ms.dialogue,
      "Conversation" -> ms.multilogue,
      "Not a natural format" -> ms.notANaturalFormatConversationalType),
    objectMap(cmd.Channel, imdi.channel,
      "Unknown" -> imdi.unknownChannel,
      "Face to Face" -> imdi.faceToFace,
      "Experimental Setting" -> imdi.experimentalSetting,
      "Broadcasting" -> imdi.broadcasting,
      "Telephone" -> imdi.telephoneChannel,
      "wizard-of-oz" -> imdi.wizardOfOz,
      "Human-machine dialogue" -> imdi.humanMachineDialogue,
      "Other" -> imdi.otherChannel)
  )

  cmd.Content_Languages --> (
    imdi.contentLanguages > node(frag("ContentLanguages")) (
      handle(cmd.descriptions),
      handle(cmd.Content_Language)
    )
  )

  def trinaryMap(node : NodeRequest, prop : NodeRequest) = {
    forall(node)(
      when(content === "true")(
        prop > true
      ).or(content === "false")(
        prop > false
      ).or(content === "Unknown")(
        prop > text("unknown")
      )
    )
  }

  cmd.Content_Language --> (
    imdi.contentLanguage > node(frag("ContentLanguage")) (
      stringMap(cmd.Id, dc.identifier),
      stringMap(cmd.Name, rdfs.label),
      trinaryMap(cmd.Dominant, imdi.dominant),
      trinaryMap(cmd.SourceLanguage, imdi.sourceLanguage),
      trinaryMap(cmd.TargetLanguage, imdi.targetLanguage),
      handle(cmd.descriptions)
    )
  )

  cmd.Actors --> (
    imdi.actors > node(frag("Actors"))(
      handle(cmd.descriptions),
      handle(cmd.Actor)
    )
  )

  cmd.Actor --> (
    imdi.actor > node(frag("Actor"))(
      a > foaf.Person,
      stringMap(cmd.Role, imdi.role),
      stringMap(cmd.Name, foaf.name),
      stringMap(cmd.FullName, imdi.fullName),
      stringMap(cmd.Code, imdi.code),
      stringMap(cmd.FamilySocialRole, imdi.familySocialRole),
      stringMap(cmd.EthnicGroup, imdi.ethnicGroup),
      intMap(cmd.Age, foaf.age),
      dateMap(cmd.BirthDate, foaf.birthday),
      objectMap(cmd.Sex, ms.sex,
        "Male" -> ms.male,
        "Female" -> ms.female,
        "NAP" -> imdi.NAP,
        "Unknown" -> cmd.unknownSex),
      stringMap(cmd.Education, imdi.education),
      trinaryMap(cmd.Anonymized, imdi.anonymized),
      handle(cmd.Contact),
      handle(cmd.Keys),
      handle(cmd.descriptions),
      handle(cmd.Actor_Languages)
    )
  )

  cmd.Actor_Languages --> (
    imdi.actorLanguages > node(frag("ActorLanguages"))(
      handle(cmd.descriptions),
      handle(cmd.Actor_Language)
    )
  )

  cmd.Actor_Language --> (
    imdi.actorLanguage > node(frag("ActorLanguage"))(
      stringMap(cmd.Name, rdfs.label),
      trinaryMap(cmd.MotherTongue, imdi.motherTongue),
      trinaryMap(cmd.PrimaryLanguage, imdi.primaryLanguage),
      handle(cmd.descriptions)
    )
  )

  cmd.Resources --> (
    handle(cmd.MediaFile),
    handle(cmd.WrittenResource),
    handle(cmd.Source),
    handle(cmd.Anonyms)
  )

  cmd.MediaFile --> (
    imdi.mediaFile > node(frag("MediaFile"))(
      linkMap(cmd.ResourceLink, imdi.resourceLink),
      objectMap(cmd.Type, dct.`type`,
        "Unknown" -> imdi.unknownMediaFileType,
        "audio" -> imdi.audioMediaFileType,
        "video" -> imdi.videoMediaFileType,
        "image" -> imdi.imageMediaFileType,
        "drawing" -> imdi.drawingMediaFileType,
        "document" -> imdi.documentMediaFileType,
        "text" -> imdi.textMediaFileType),
      stringMap(cmd.Format, dc.format),
      stringMap(cmd.Size, imdi.size),
      objectMap(cmd.Quality, imdi.mediaFileQuality,
        "Unknown" -> imdi.unknownQuality,
        "1" -> imdi.quality_1,
        "2" -> imdi.quality_2,
        "3" -> imdi.quality_3,
        "4" -> imdi.quality_4,
        "5" -> imdi.quality_5),
      stringMap(cmd.RecordingConditions, imdi.recordingConditions),
      handle(cmd.TimePosition),
      handle(cmd.Access),
      handle(cmd.descriptions),
      handle(cmd.Keys)
    )
  )

  cmd.TimePosition --> (
    stringMap(cmd.Start, imdi.startTimePosition),
    stringMap(cmd.End, imdi.endTimePosition)
  )

  cmd.Access --> (
    imdi.access > node(frag("Access"))(
      stringMap(cmd.Availability, imdi.availability),
      dateMap(cmd.Date, dc.date),
      stringMap(cmd.Owner, imdi.owner),
      stringMap(cmd.Publisher, dc.publisher),
      handle(cmd.Contact),
      handle(cmd.descriptions)
    )
  )

  cmd.WrittenResource --> (
    imdi.writtenResource > node(frag("WrittenResource"))(
      linkMap(cmd.ResourceLink, imdi.resourceLink),
      stringMap(cmd.MediaResourceLink, imdi.mediaResourceLink),
      dateMap(cmd.Date, dc.date),
      stringMap(cmd.Type, dc.`type`),
      stringMap(cmd.SubType, dc.`subtype`),
      stringMap(cmd.Format, dc.format),
      stringMap(cmd.Size, imdi.size),
      objectMap(cmd.Derivation, imdi.derivation,
        "Unknown" -> imdi.unknownDerivation,
        "Original" -> imdi.originalDerivation,
        "Analysis" -> imdi.analysisDerivation,
        "Translation" -> imdi.translationDerivation,
        "Commentary" -> imdi.commentaryDerivation,
        "Criticism" -> imdi.criticismDerivation,
        "Annotation" -> imdi.annotationDerivation),
      stringMap(cmd.CharacterEncoding, ms.characterEncoding),
      stringMap(cmd.ContentEncoding, ms.contentEncoding),
      stringMap(cmd.LanguageId, ms.languageId),
      trinaryMap(cmd.Anonymized, imdi.anonymized),
      handle(cmd.Validation),
      handle(cmd.Access),
      handle(cmd.descriptions),
      handle(cmd.Keys)
    )
  )

  cmd.Validation --> (
    forall(cmd.Type)(
      when(content === "Unknown")(
        ms.validationType > imdi.unknownValidationType
      ).or(content === "Formal")(
        ms.validationType > ms.formal
      ).or(content === "Content")(
        ms.validationType > ms.content
      ).or(content === "Formal/Content")(
        ms.validationType > ms.formal,
        ms.validationType > ms.content
      ).or(content === "None")(
        ms.validationType > imdi.noValidation
      )
    ),
    objectMap(cmd.Methodology, ms.validationMode,
      "Unknown" -> imdi.unknownValidationMethodology,
      "Hand" -> ms.manual,
      "Automatic" -> ms.automatic,
      "Semi-Automatic" -> ms.mixed),
    stringMap(cmd.Level, imdi.validationLevel),
    handle(cmd.descriptions)
  )


  cmd.Source --> (
    imdi.source > node(frag("Source"))(
      stringMap(cmd.Id, dc.identifier),
      stringMap(cmd.Format, dc.format),
      objectMap(cmd.Quality, imdi.sourceQuality,
        "Unknown" -> imdi.unknownQuality,
        "1" -> imdi.quality_1,
        "2" -> imdi.quality_2,
        "3" -> imdi.quality_3,
        "4" -> imdi.quality_4,
        "5" -> imdi.quality_5),
      handle(cmd.CounterPosition),
      handle(cmd.TimePosition),
      handle(cmd.Access),
      handle(cmd.descriptions),
      handle(cmd.Keys)
    )
  )

  cmd.CounterPosition --> (
    stringMap(cmd.Start, imdi.startCounterPosition),
    stringMap(cmd.End, imdi.endCounterPosition)
  )

  cmd.Anonyms --> (
    imdi.anonyms > node(frag("Anonyms"))(
      linkMap(cmd.ResourceLink, imdi.ResourceLink),
      handle(cmd.Access)
    )
  )

  cmd.References --> (
    forall(cmd.descriptions)(
      imdi.reference > content
    )
  )
}
