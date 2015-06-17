import eu.liderproject.lixr._
import scala.language.dynamics

new ModelWithMappings {
  val cc = Namespace("http://creativecommons.org/ns/")
  val dcat = Namespace("http://www.w3.org/ns/dcat#")
  val dc = Namespace("http://purl.org/dc/elements/1.1/")
  val dct = Namespace("http://purl.org/dc/terms/")
  val foaf = Namespace("http://xmlns.com/foaf/0.1/")
  val media = Namespace("http://www.w3.org/ns/ma-ont#")
  val ms = Namespace("http://purl.org/net/def/metashare#")
  val msxml = Namespace("http://www.ilsp.gr/META-XMLSchema")
  val oai = Namespace("http://www.openarchives.org/OAI/2.0/")
  val odrl = Namespace("http://www.w3.org/ns/odrl/2/")
  val swrc = Namespace("http://swrc.ontoware.org/ontology#")

  oai.`OAI-PMH` --> (
    handle(oai.responseDate),
    handle(oai.request),
    handle(oai.ListRecords)
  )

  //oai.responseDate --> text("2014-10-03T11:18:32Z")

  oai.request --> (
    xmlAtt("verb","ListRecords"),
    xmlAtt("metadataPrefix","metashare")//,
//    text("http://127.0.0.1:8000/oai_pmh/")
  )

  oai.ListRecords --> 
    handle(oai.record)
  
  oai.record --> (
    set("resourceID",content(oai.header \ oai.identifier) or uuid) (
      handle(oai.header),
      handle(oai.metadata),
      handle(oai.about)
    )
  )

  oai.header.when(current.att("status") === "deleted") --> (
    comment("deleted " +: content(oai.identifier))
  )

  oai.header --> (
    node(get("resourceID") :+ "#Header") (
      a > dcat.CatalogRecord,
      dateMap(oai.datestamp, dct.issued),
      stringMap(oai.setSpec, ms.setSpec),
      foaf.primaryTopic > node(get("resourceID"))()
    )
  )

  oai.about --> fail("Did not expect an about tag")

  oai.metadata --> handle(msxml.resourceInfo)
    
  def personAsContributor = forall(msxml.personInfo)(dc.contributor > (content(msxml.givenName) :+ " " +: content(msxml.surname)))
  def personAsCreator = forall(msxml.personInfo)(dc.creator > (content(msxml.givenName) :+ " " +: content(msxml.surname)))

  // Based on META-SHARE-Resource.xsd

  msxml.resourceInfo --> node(get("resourceID")) (
    rdf_type > ms.LanguageResource,
    handle(msxml.identificationInfo),
    handle(msxml.distributionInfo),
    handle(msxml.contactPerson),
    handle(msxml.metadataInfo),
    handle(msxml.versionInfo),
    handle(msxml.validationInfo),
    handle(msxml.usageInfo),
    handle(msxml.resourceDocumentationInfo),
    handle(msxml.resourceCreationInfo),
    handle(msxml.relationInfo),
    handle(msxml.resourceComponentType),
    rdfs.seeAlso > node("http://metashare.elda.org/repository/browse/" +: get("resourceID") :+ "/")(),
    rdf_type > dcat.Dataset,
    // Basic DC properties
    dc.source > "META-SHARE",
    forall(msxml.validationInfo) (
      forall(msxml.validator) (
        personAsContributor
      )
    ),
    forall(msxml.resourceCreationInfo) (
      forall(msxml.resourceCreator) (
        personAsCreator
      )
    ),
    forall(msxml.resourceComponentType) (
      forall(msxml.corpusInfo) (
        forall(msxml.corpusMediaType) (
          forall(msxml.corpusVideoInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.videoClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          ),
          forall(msxml.corpusAudioInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.audioClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          ),
          forall(msxml.corpusImageInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.imageClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          ),
          forall(msxml.corpusTextInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.textClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          ),
          forall(msxml.corpusTextNgramInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.textClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          ),
          forall(msxml.corpusTextNumericalInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.corpusVideoInfo) (
            forall(msxml.annotationInfo) (
              forall(msxml.annotator) (
                personAsContributor
              )
            ),
            forall(msxml.recordingInfo) (
              forall(msxml.recorder) (
                personAsContributor
              )
            ),
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            ),
            forall(msxml.videoClassificationInfo) (
              forall(msxml.subject_topic) (
                dc.subject > content
              )
            )
          )
        )
      ),
      forall(msxml.languageDescriptionInfo) (
        forall(msxml.languageDescriptionMediaType) (
          forall(msxml.languageDescriptionTextInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.languageDescriptionImageInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.languageDescriptionVideoInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          )
        )
      ),
      forall(msxml.lexicalConceptualResourceInfo) (
        forall(msxml.lexicalConceptualResourceMediaType) (
          forall(msxml.lexicalConceptualResourceAudioInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.lexicalConceptualResourceImageInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.lexicalConceptualResourceTextInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          ),
          forall(msxml.lexicalConceptualResourceVideoInfo) (
            forall(msxml.languageInfo) (
              forall(msxml.languageId) (
                dc.language > content
              )
            )
          )
        )
      )
    ),
    forall(msxml.resourceComponentType) (
      forall(msxml.toolServiceInfo) (
        forall(msxml.toolServiceEvaluationInfo) (
          forall(msxml.evaluator) (
            personAsContributor
          )
        )
      )
    ),
    forall(msxml.identificationInfo) (
      langStringMap(msxml.description, dc.description)
    ),
    forall(msxml.distributionInfo) (
      forall(msxml.licenceInfo) (
        forall(msxml.licence) (
          dc.rights > content
        )
      )
    )
  )

  msxml.resourceComponentType --> (
    handle(msxml.corpusInfo),
    handle(msxml.toolServiceInfo),
    handle(msxml.languageDescriptionInfo),
    handle(msxml.lexicalConceptualResourceInfo)
  )

  msxml.contactPerson --> (
    dcat.contactPoint > node(frag("contactPerson")) (
      personInfoType,
      rdf_type > ms.ContactPerson,
      rdf_type > foaf.Person
    )
  )

  // Based on META-SHARE-BaseTypes.xsd

  msxml.identificationInfo --> (
    langStringMap(msxml.resourceName,dc.title),
    langStringMap(msxml.description,dc.description),
    langStringMap(msxml.resourceShortName,ms.resourceShortName),
    stringMap(msxml.metaShareId,ms.metaShareId),
    handle(msxml.url),
    stringMap(msxml.identifier,dc.identifier)
  )

  msxml.url --> (
    ms.url > node(content)()
  )
    
  msxml.versionInfo --> (
    stringMap(msxml.version, dct.hasVersion),
    stringMap(msxml.revision, ms.revision),
    dateMap(msxml.lastDateUpdated, dct.modified),
    stringMap(msxml.updateFrequency, ms.updateFrequency)
  )

  msxml.validationInfo --> (
    ms.validationInfo > node(frag("validationInfo")) (
      rdf_type > ms.ValidationInfo,
      boolMap(msxml.validated, ms.validated),
      objectMap(msxml.validationType, ms.validationType,
        "formal" -> ms.formal,
        "content" -> ms.content),
      objectMap(msxml.validationMode, ms.validationMode,
        "manual" -> ms.manual,
        "automatic" -> ms.automatic,
        "mixed" -> ms.mixed,
        "interactive" -> ms.interactive),
      stringMap(msxml.validationModeDetails, ms.validationModeDetails),
      objectMap(msxml.validationExtent, ms.validationExtent,
        "full" -> ms.full,
        "partial" -> ms.partial
        ),
      stringMap(msxml.validationExtentDetails, ms.validationExtentDetails),
      handle(msxml.sizePerValidation),
      handle(msxml.validationReport),
      handle(msxml.validationTool),
      handle(msxml.validator)    
    )
  )

  msxml.sizePerValidation --> (
    ms.sizePerValidation > node(frag("sizePerValidation")) (
      sizeInfoType
    )
  )

  msxml.validationReport --> (
    ms.validationReport > node(frag("validationReport")) (
      documentationInfoType
    )
  )

  msxml.validationTool --> (
    ms.validationTool > targetResourceInfoType
  )

  msxml.validator --> (
    ms.validator > node(frag("validator")) (
      actorInfoType
    )
  )

  msxml.resourceCreationInfo --> (
    handle(msxml.resourceCreator),
    handle(msxml.fundingProject),
    dateMap(msxml.creationStartDate,ms.creationStartDate),
    dateMap(msxml.creationEndDate,ms.creationEndDate)
  )

  msxml.resourceCreator --> (
    dct.creator > node(frag("resourceCreator")) (
      actorInfoType
    )
  )

  msxml.fundingProject --> (
    ms.fundingProject > node(frag("fundingProject")) (
      projectInfoType
    )
  )

  msxml.creationInfo --> (
    handle(msxml.originalSource),
    objectMap(msxml.creationMode, ms.creationMode,
      "manual" -> ms.manual,
      "automatic" -> ms.automatic,
      "mixed" -> ms.mixed,
      "interactive" -> ms.interactive),
    stringMap(msxml.creationModeDetails, ms.creationModeDetails),
    handle(msxml.creationTool)
  )

  msxml.originalSource --> (
    ms.originalSource > targetResourceInfoType
  )

  msxml.creationTool --> (
    ms.creationTool >  targetResourceInfoType
  )

  msxml.metadataInfo --> (
    foaf.primaryTopic < node(frag("metadataInfo")) (
      rdf_type > ms.MetadataInfo,
      rdf_type > dcat.CatalogRecord,
      dateMap(msxml.metadataCreationDate, dct.created),
      handle(msxml.metadataCreator),
      stringMap(msxml.source, dc.source),
      stringMap(msxml.originalMetadataSchema, ms.originalMetadataSchema),
      linkMap(msxml.originalMetadataLink, ms.originalMetadataLink),
      stringMap(msxml.metadataLanguageName, ms.languageName),
      stringMap(msxml.metadataLanguageId, dc.language),
      dateMap(msxml.metadataLastDateUpdated, dct.modified),
      stringMap(msxml.revision, ms.revision)
    )
  )

  msxml.metadataCreator --> (
    dct.creator > node(frag("metadataCreator")) (
      actorInfoType
    )
  )

  msxml.documentInfo --> (
    ms.documentInfo > node(frag("documentInfo")) (
      rdf_type > ms.DocumentInfo,
      objectMap(msxml.documentType, rdf_type,
        "article" -> swrc.Article,
        "book" -> swrc.Book,
        "booklet" -> swrc.Booklet,
        "manual" -> swrc.Manual,
        "techReport" -> swrc.TechReport,
        "mastersThesis" -> swrc.MastersThesis,
        "phdThesis" -> swrc.PhDThesis,
        "inBook" -> swrc.InBook,
        "inCollection" -> swrc.InCollection,
        "proceedings" -> swrc.Proceedings,
        "inProceedings" -> swrc.InProceedings,
        "unpublished" -> swrc.Unpublished,
        "other" -> swrc.Other),
      stringMap(msxml.title, swrc.title),
      stringMap(msxml.author, swrc.creator),
      stringMap(msxml.editor, swrc.editor),
      yearMap(msxml.editor, swrc.year),
      stringMap(msxml.publisher, swrc.publisher),
      stringMap(msxml.bookTitle, swrc.booktitle),
      stringMap(msxml.journal, swrc.journal),
      stringMap(msxml.volume, swrc.volume),
      stringMap(msxml.series, swrc.series),
      stringMap(msxml.pages, swrc.pages),
      stringMap(msxml.edition, swrc.edition),
      stringMap(msxml.conference, swrc.conference),
      stringMap(msxml.doi, swrc.doi),
      linkMap(msxml.url, rdfs.seeAlso),
      stringMap(msxml.ISSN, ms.issn),
      stringMap(msxml.ISBN, swrc.isbn),
      stringMap(msxml.keywords, swrc.keywords),
      stringMap(msxml.documentLanguageName, ms.languageName),
      stringMap(msxml.documentLanguageId, dc.language)
    )
  )

  msxml.documentationInfo --> (
    ms.documentationInfo > node(frag("documentationInfo")) (
      documentationInfoType
    )
  )

  def documentationInfoType = ( 
    (rdf_type > ms.Documentation) ++
    handle(msxml.documentUnstructured) ++
    handle(msxml.documentInfo)
  )

  msxml.resourceDocumentationInfo --> (
    handle(msxml.documentation),
    linkMap(msxml.samplesLocation, ms.samplesLocation),
    objectMap(msxml.toolDocumentationType, ms.toolDocumentationType,
      "online" -> ms.online,
      "manual" -> ms.manual,
      "helpFunctions" -> ms.helpFunctions,
      "none" -> ms.none,
      "other" -> ms.other)      
  )

  msxml.documentation --> (
    ms.documentation > node(frag("documentation")) (
      documentationInfoType
    )
  )

  msxml.domainInfo --> (
    ms.domainInfo > node(frag("domainInfo")) (
      rdf_type > ms.DomainInfo,
      stringMap(msxml.domain, ms.domain),
      handle(msxml.sizePerDomain),
      conformanceToClassificationScheme
    )
  )

  msxml.sizePerDomain --> (
    ms.sizePerDomain > node(frag("sizePerDomain")) (
      sizeInfoType
    )
  )

  msxml.annotationInfo --> (
    ms.annotationInfo > node(frag("annotationInfo")) (
      rdf_type > ms.AnnotationInfo,
      annotationType,
      objectMap(msxml.annotatedElements, ms.annotatedElements,
        "speakerNoise" -> ms.speakerNoise,
        "backgroundNoise" -> ms.backgroundNoise,
        "mispronunciations" -> ms.mispronunciations,
        "truncation" -> ms.truncation,
        "discourseMarkers" -> ms.discourseMarkers,
        "other" -> ms.other
      ),
      boolMap(msxml.annotationStandoff, ms.annotationStandoff),
      segmentationLevel,
      annotationFormat,
      tagset,
      stringMap(msxml.tagsetLanguageId, ms.tagsetLanguageId),
      stringMap(msxml.tagsetLanguageName, ms.tagsetLanguageName),
      conformanceToStandardsBestPractices,
      stringMap(msxml.theoreticModel, ms.theoreticModel),
      handle(msxml.annotationManual),
      objectMap(msxml.annotationMode, ms.annotationMode,
        "automatic" -> ms.automatic,
        "manual" -> ms.manual,
        "mixed" -> ms.mixed,
        "interactive" -> ms.interactive
        ),
      stringMap(msxml.annotationModeDetails, ms.annotationModeDetails),
      handle(msxml.annotationTool),
      dateMap(msxml.annotationStartDate, ms.annotationStartDate),
      dateMap(msxml.annotationEndDate, ms.annotationEndDate),
      handle(msxml.sizePerAnnotation),
      stringMap(msxml.interannotatorAgreement, ms.interannotatorAgreement),
      stringMap(msxml.intraannotatorAgreement, ms.intraannotatorAgreement),
      handle(msxml.annotator)
    )
  )

  msxml.annotationTool --> (
    ms.annotationTool > targetResourceInfoType
  )

  msxml.sizePerAnnotation --> (
    ms.sizePerAnnotation > node(frag("sizePerAnnotation")) (
      sizeInfoType
    )
  )

  msxml.annotator --> (
    ms.annotator > node(frag("annotator")) (
      actorInfoType
    )
  )

  msxml.annotationManual --> (
    ms.annotationManual > node(frag("annotationManual")) (
      documentationInfoType
    )
  )
      
  def targetResourceInfoType = node(content(msxml.targetResourceNameURI))()

  msxml.relationInfo --> (
    ms.relationInfo > node(frag("relationInfo")) (
      rdf_type > ms.RelationInfo,
      stringMap(msxml.relationType, ms.relationType),
      handle(msxml.relatedResource)
    )
  )

  msxml.relatedResource --> (
    ms.relatedResource > targetResourceInfoType
  )

  msxml.modalityInfo --> (
    ms.modalityInfo > node(frag("modalityInfo")) (
      rdf_type > ms.ModalityInfo,
      modalityType,
      stringMap(msxml.modalityTypeDetails, ms.modalityTypeDetails),
      handle(msxml.sizePerModality)
    )
  )

  msxml.sizePerModality --> (
    ms.sizePerModality > node(frag("sizePerModality")) (
      sizeInfoType
    )
  )

  msxml.participantInfo --> (
    ms.participantInfo > node(frag("participantInfo")) (
      rdf_type > ms.ParticipantInfo,
      rdf_type > foaf.Person,
      langStringMap(msxml.alias, ms.alias),
      objectMap(msxml.ageGroup, ms.ageGroup,
        "child" -> ms.child,
        "teenager" -> ms.teenager,
        "adult" -> ms.adult,
        "elderly" -> ms.elderly
        ),
      stringMap(msxml.age, foaf.age),
      stringMap(msxml.sex, foaf.gender),
      objectMap(msxml.origin, ms.origin,
        "native" -> ms.native,
        "nonNative" -> ms.nonNative,
        "unknown" -> ms.unknown),
      stringMap(msxml.placeOfLiving, foaf.based_near),
      stringMap(msxml.placeOfBirth, ms.placeOfBirth),
      stringMap(msxml.placeOfChildhood, ms.placeOfChildhood),
      langStringMap(msxml.dialectAccent, ms.dialectAccent),
      stringMap(msxml.speakingImpairment, ms.speakingImpairment),
      stringMap(msxml.hearingImpairment, ms.hearingImpairment),
      stringMap(msxml.smokingHabits, ms.smokingHabits),
      objectMap(msxml.vocalTractConditions, ms.vocalTractConditions,
        "dentalProsthesis" -> ms.dentalProsthesis,
        "other" -> ms.other),
      stringMap(msxml.profession, ms.profession),
      intMap(msxml.height, ms.height),
      intMap(msxml.weight, ms.weight),
      boolMap(msxml.trainedSpeaker, ms.trainedSpeaker),
      stringMap(msxml.placeOfSecondEducation, ms.placeOfSecondEducation),
      stringMap(msxml.educationLevel, ms.educationLevel)
    )
  )

  msxml.captureInfo --> (
    objectMap(msxml.capturingDeviceType, ms.capturingDeviceType,
      "studioEquipment" -> ms.studioEquipment,
      "microphone" -> ms.microphone,
      "closeTalkMicrophone" -> ms.closeTalkMicrophone,
      "farfieldMicrophone" -> ms.farfieldMicrophone,
      "lavalierMicrophone" -> ms.lavalierMicrophone,
      "microphoneArray" -> ms.microphoneArray,
      "embeddedMicrophone" -> ms.embeddedMicrophone,
      "largeMembraneMicrophone" -> ms.largeMembraneMicrophone,
      "laryngograph" -> ms.laryngograph,
      "telephoneFixed" -> ms.telephoneFixed,
      "telephoneMobile" -> ms.telephoneMobile,
      "telephoneIP" -> ms.telephoneIP,
      "camera" -> ms.camera,
      "webcam" -> ms.webcam,
      "other" -> ms.other
      ),
    stringMap(msxml.capturingDeviceTypeDetails, ms.capturingDeviceTypeDetails),
    stringMap(msxml.capturingDetails, ms.capturingDetails),
    objectMap(msxml.capturingEnvironment, ms.capturingEnvironment,
      "complex" -> ms.complex,
      "plain" -> ms.plain
      ),
    stringMap(msxml.sensorTechnology, ms.sensorTechnology),
    objectMap(msxml.sceneIllumination, ms.sceneIllumination,
      "daylight" -> ms.daylight,
      "fix" -> ms.fix,
      "multipleSources" -> ms.multipleSources,
      "singleSource" -> ms.singleSource,
      "variable" -> ms.variable,
      "other" -> ms.other
      ),
    handle(msxml.personSourceSetInfo)
  )

  msxml.personSourceSetInfo --> (
    intMap(msxml.numberOfPersons, ms.numberOfPersons),
    objectMap(msxml.ageOfPersons, ms.ageOfPersons,
      "child" -> ms.child,
      "teenager" -> ms.teenager,
      "adult" -> ms.adult,
      "elderly" -> ms.elderly
      ),
    intMap(msxml.ageRangeStart, ms.ageRangeStart),
    intMap(msxml.ageRangeEnd, ms.ageRangeEnd),
    objectMap(msxml.sexOfPersons, ms.sexOfPersons,
      "male" -> ms.male,
      "female" -> ms.female,
      "mixed" -> ms.mixed,
      "unknown" -> ms.unknown
      ),
    objectMap(msxml.originOfPersons, ms.originOfPersons,
      "native" -> ms.native,
      "nonNative" -> ms.nonNative,
      "mixed" -> ms.mixed,
      "unknown" -> ms.unknown
      ),
    stringMap(msxml.dialectAccentOfPersons, ms.dialectAccentOfPersons),
    stringMap(msxml.geographicDistributionOfPersons, ms.geographicDistributionOfPersons),
    objectMap(msxml.hearingImpairmentOfPersons, ms.hearingImpairmentOfPersons,
      "yes" -> ms.yes,
      "no" -> ms.no,
      "mixed" -> ms.mixed
      ),
    objectMap(msxml.speakingImpairmentOfPersons, ms.speakingImpairmentOfPersons,
      "yes" -> ms.yes,
      "no" -> ms.no,
      "mixed" -> ms.mixed
      ),
    intMap(msxml.numberOfTrainedSpeakers, ms.numberOfTrainedSpeakers),
    objectMap(msxml.speechInfluences, ms.speechInfluences,
      "alcohol" -> ms.alcohol,
      "sleepDeprivation" -> ms.sleepDeprivation,
      "hyperbaric" -> ms.hyperbaric,
      "medication" -> ms.medication,
      "other" -> ms.other
      ),
    handle(msxml.participantInfo)
  )

  msxml.settingInfo --> (
    objectMap(msxml.naturality, ms.naturality,
      "natural" -> ms.natural,
      "planned" -> ms.planned,
      "semiPlanned" -> ms.semiPlanned,
      "readSpeech" -> ms.readSpeech,
      "spontaneous" -> ms.spontaneous,
      "elicited" -> ms.elicited,
      "assisted" -> ms.assisted,
      "prompted" -> ms.prompted,
      "other" -> ms.other
      ),
    objectMap(msxml.conversationalType, ms.conversationalType,
      "monologue" -> ms.monologue,
      "dialogue" -> ms.dialogue,
      "multilogue" -> ms.multilogue
      ),
    objectMap(msxml.scenarioType, ms.scenarioType,
      "frogStory" -> ms.frogStory,
      "pearStory" -> ms.pearStory,
      "mapTask" -> ms.mapTask,
      "onlineEducationalGame" -> ms.onlineEducationalGame,
      "pearStory" -> ms.pearStory,
      "rolePlay" -> ms.rolePlay,
      "wordGame" -> ms.wordGame,
      "wizardOfOz" -> ms.wizardOfOz,
      "other" -> ms.other
      ),
    objectMap(msxml.audience, ms.audience,
      "no" -> ms.no,
      "few" -> ms.few,
      "some" -> ms.some,
      "largePublic" -> ms.largePublic
      ),
    objectMap(msxml.interactivity, ms.interactivity,
      "interactive" -> ms.interactive,
      "nonInteractive" -> ms.nonInteractive,
      "semiInteractive" -> ms.semiInteractive,
      "overlapping" -> ms.overlapping,
      "other" -> ms.other
      ),
    stringMap(msxml.interaction, ms.interaction)
  )

  msxml.runningEnvironmentInfo --> (
    handle(msxml.requiredSoftware),
    objectMap(msxml.requiredHardware, ms.requiredHardware,
      "graphicCard" -> ms.graphicCard,
      "microphone" -> ms.microphone,
      "ocrSystem" -> ms.ocrSystem,
      "specialHardwareEquipment" -> ms.specialHardwareEquipment,
      "none" -> ms.none,
      "other" -> ms.other
      ),
    handle(msxml.requiredLRs),
    stringMap(msxml.runningEnvironmentDetails, ms.runningEnvironmentDetails)
  )

  msxml.requiredSoftware --> (
    ms.requiredSoftware > targetResourceInfoType
  )

  msxml.requiredLRs --> (
    ms.requiredLRs > targetResourceInfoType
  )

  msxml.recordingInfo --> (
    objectMap(msxml.recordingDeviceType, ms.recordingDeviceType,
      "hardDisk" -> ms.hardDisk,
      "dv" -> ms.dv,
      "tapeVHS" -> ms.tapeVHS,
      "flash" -> ms.flash,
      "DAT" -> ms.DAT,
      "soundBlasterCard" -> ms.soundBlasterCard,
      "other" -> ms.other
      ),
    stringMap(msxml.recordingDeviceTypeDetails, ms.recordingDeviceTypeDetails),
    stringMap(msxml.recordingPlatformSoftware, ms.recordingPlatformSoftware),
    objectMap(msxml.recordingEnvironment, ms.recordingEnvironment,
      "office" -> ms.office,
      "inCar" -> ms.inCar,
      "studio" -> ms.studio,
      "conferenceRoom" -> ms.conferenceRoom,
      "lectureRoom" -> ms.lectureRoom,
      "industrial" -> ms.industrial,
      "transport" -> ms.transport,
      "openPublicPlace" -> ms.openPublicPlace,
      "closedPublicPlace" -> ms.closedPublicPlace,
      "anechoicChamber" -> ms.anechoicChamber,
      "other" -> ms.other
      ),
    objectMap(msxml.sourceChannel, ms.sourceChannel,
      "internet" -> ms.internet,
      "radio" -> ms.radio,
      "tv" -> ms.tv,
      "telephone" -> ms.telephone,
      "laryngograph" -> ms.laryngograph,
      "airflow" -> ms.airflow,
      "EMA" -> ms.EMA,
      "webCam" -> ms.webCam,
      "camcorder" -> ms.camcorder,
      "other" -> ms.other
      ),
    objectMap(msxml.sourceChannelType, ms.sourceChannelType,
      "ISDN" -> ms.ISDN,
      "GSM" -> ms.GSM,
      "3G" -> ms.`3G`,
      "CDMA" -> ms.CDMA,
      "DVB-T" -> ms.`DVB-T`,
      "DVB-S" -> ms.`DVB-S`,
      "DVB-C" -> ms.`DVB-C`,
      "VOIP" -> ms.VOIP,
      "other" -> ms.other
      ),
    stringMap(msxml.sourceChannelName, ms.sourceChannelName),
    stringMap(msxml.sourceChannelDetails, ms.sourceChannelDetails),
    handle(msxml.recorder)
  )

  msxml.recorder --> (
    ms.recorder > node(frag("recorder")) (
      actorInfoType
    )
  )

  msxml.resolutionInfo --> (
    ms.resolutionInfo > node(frag("resolutionInfo")) (
      rdf_type > ms.ResolutionInfo,
      intMap(msxml.sizeWidth, ms.sizeWidth),
      intMap(msxml.sizeHeight, ms.sizeHeight),
      objectMap(msxml.resolutionStandard, ms.resolutionStandard,
        "VGA" -> ms.VGA,
        "HD.720" -> ms.`HD.720`,
        "HD.1080" -> ms.`HD.1080`
        )
    )
  )

  msxml.compressionInfo --> (
    boolMap(msxml.compression, ms.compression),
    objectMap(msxml.compressionName, ms.compressionName,
      "mpg" -> ms.mpg,
      "avi" -> ms.avi,
      "mov" -> ms.mov,
      "flac" -> ms.flac,
      "shorten" -> ms.shorten,
      "mp3" -> ms.mp3,
      "oggVorbis" -> ms.oggVorbis,
      "atrac" -> ms.atrac,
      "aac" -> ms.aac,
      "mpeg" -> ms.mpeg,
      "realAudio" -> ms.realAudio,
      "other" -> ms.other
      ),
    boolMap(msxml.compressionLoss, ms.compressionLoss)
  )

  msxml.linkToOtherMediaInfo --> (
    ms.linkToOtherMediaInfo > node(frag("linkToOtherMediaInfo")) (
      rdf_type > ms.LinkToOtherMediaInfo,
      objectMap(msxml.otherMedia, ms.otherMedia,
        "text" -> ms.text,
        "textNumerical" -> ms.textNumerical,
        "video" -> ms.video,
        "audio" -> ms.audio,
        "image" -> ms.image
        ),
      stringMap(msxml.mediaTypeDetails, ms.mediaTypeDetails),
      boolMap(msxml.synchronizedWithText, ms.synchronizedWithText),
      boolMap(msxml.synchronizedWithAudio, ms.synchronizedWithAudio),
      boolMap(msxml.synchronizedWithVideo, ms.synchronizedWithVideo),
      boolMap(msxml.sycnhronizedWithImage, ms.synchronizedWithImage),
      boolMap(msxml.synchronizedWithTextNumerical, ms.synchronizedWithTextNumerical)
    )
  )

  msxml.documentList --> (
    ms.documentList > node(frag("documentList")) (
      rdf_type > ms.DocumentList,
      handle(msxml.documentInfo)
    )
  )

  def conformanceToStandardsBestPractices = objectMap(msxml.conformanceToStandardsBestPractices, ms.conformanceToStandardsBestPractices,
    "BLM" -> ms.BLM,
    "CES" -> ms.CES,
    "EAGLES" -> ms.EAGLES,
    "EML" -> ms.EML,
    "EMMA" -> ms.EMMA,
    "GMX" -> ms.GMX,
    "GrAF" -> ms.GrAF,
    "HamNoSys" -> ms.HamNoSys,
    "InkML" -> ms.InkML,
    "ISO12620" -> ms.ISO12620,
    "ISO16642" -> ms.ISO16642,
    "ISO1987" -> ms.ISO1987,
    "ISO26162" -> ms.ISO26162,
    "ISO30042" -> ms.ISO30042,
    "ISO704" -> ms.ISO704,
    "LMF" -> ms.LMF,
    "MAF" -> ms.MAF,
    "MLIF" -> ms.MLIF,
    "MULTEXT" -> ms.MULTEXT,
    "MUMIN" -> ms.MUMIN,
    "multimodalInteractionFramework" -> ms.multimodalInteractionFramework,
    "OAXAL" -> ms.OAXAL,
    "OWL" -> ms.OWL,
    "pennTreeBank" -> ms.pennTreeBank,
    "pragueTreebank" -> ms.pragueTreebank,
    "RDF" -> ms.RDF,
    "SemAF" -> ms.SemAF,
    "SemAF_DA" -> ms.SemAF_DA,
    "SemAF_NE" -> ms.SemAF_NE,
    "SemAF_SRL" -> ms.SemAF_SRL,
    "SemAF_DS" -> ms.SemAF_DS,
    "SKOS" -> ms.SKOS,
    "SRX" -> ms.SRX,
    "SynAF" -> ms.SynAF,
    "TBX" -> ms.TBX,
    "TMX" -> ms.TMX,
    "TEI" -> ms.TEI,
    "TEI_P3" -> ms.TEI_P3,
    "TEI_P4" -> ms.TEI_P4,
    "TEI_P5" -> ms.TEI_P5,
    "TimeML" -> ms.TimeML,
    "XCES" -> ms.XCES,
    "XLIFF" -> ms.XLIFF,
    "WordNet" -> ms.WordNet,
    "other" -> ms.other
    )

  // Derived from META-SHARE-SimpleTypes.xsd
  
  def sizeInfoType = {
    (rdf_type > ms.Size) ++
    stringMap(msxml.size, ms.size) ++
    objectMap(msxml.sizeUnit, ms.sizeUnit,
      "terms" -> ms.terms,
      "entries" -> ms.entries,
      "turns" -> ms.turns,
      "utterances" -> ms.utterances,
      "articles" -> ms.articles,
      "files" -> ms.files,
      "items" -> ms.items,
      "seconds" -> ms.seconds,
      "elements" -> ms.elements,
      "units" -> ms.units,
      "minutes" -> ms.minutes,
      "hours" -> ms.hours,
      "texts" -> ms.texts,
      "sentences" -> ms.sentences,
      "bytes" -> ms.bytes,
      "tokens" -> ms.tokens,
      "words" -> ms.words,
      "keywords" -> ms.keywords,
      "idiomaticExpressions" -> ms.idiomaticExpressions,
      "neologisms" -> ms.neologisms,
      "multiWordUnits" -> ms.multiWordUnits,
      "expressions" -> ms.expressions,
      "synsets" -> ms.synsets,
      "classes" -> ms.classes,
      "concepts" -> ms.concepts,
      "lexicalTypes" -> ms.lexicalTypes,
      "phoneticUnits" -> ms.phoneticUnits,
      "syntacticUnits" -> ms.syntacticUnits,
      "semanticUnits" -> ms.semanticUnits,
      "predicates" -> ms.predicates,
      "phonemes" -> ms.phonemes,
      "diphones" -> ms.diphones,
      "T-HPairs" -> ms.`T-HPairs`,
      "syllables" -> ms.syllables,
      "frames" -> ms.frames,
      "images" -> ms.images,
      "kb" -> ms.kb,
      "mb" -> ms.mb,
      "gb" -> ms.gb,
      "rb" -> ms.rb,
      "shots" -> ms.shots,
      "unigrams" -> ms.unigrams,
      "bigrams" -> ms.bigrams,
      "trigrams" -> ms.trigrams,
      "4-grams" -> ms.grams,
      "5-grams" -> ms.grams,
      "rules" -> ms.rules,
      "questions" -> ms.questions,
      "other" -> ms.other
      )
  }

  def modalityType = objectMap(msxml.modalityType, ms.modalityType,
    "bodyGesture" -> ms.bodyGesture,
    "facialExpression" -> ms.facialExpression,
    "voice" -> ms.voice,
    "combinationOfModalities" -> ms.combinationOfModalities,
    "signLanguage" -> ms.signLanguage,
    "spokenLanguage" -> ms.spokenLanguage,
    "writtenLanguage" -> ms.writtenLanguage,
    "other" -> ms.other
    )
  
  def mediaType = objectMap(msxml.mediaType, ms.mediaType,
    "text" -> ms.text,
    "audio" -> ms.audio,
    "video" -> ms.video,
    "image" -> ms.image,
    "textNumerical" -> ms.textNumerical
    )

  def characterEncoding = objectMap(msxml.characterEncoding, ms.characterEncoding,
    "US-ASCII" -> ms.`US-ASCII`,
    "windows-1250" -> ms.`windows-1250`,
    "windows-1251" -> ms.`windows-1251`,
    "windows-1252" -> ms.`windows-1252`,
    "windows-1253" -> ms.`windows-1253`,
    "windows-1254" -> ms.`windows-1254`,
    "windows-1257" -> ms.`windows-1257`,
    "ISO-8859-1" -> ms.`ISO-8859-1`,
    "ISO-8859-2" -> ms.`ISO-8859-2`,
    "ISO-8859-4" -> ms.`ISO-8859-4`,
    "ISO-8859-5" -> ms.`ISO-8859-5`,
    "ISO-8859-7" -> ms.`ISO-8859-7`,
    "ISO-8859-9" -> ms.`ISO-8859-9`,
    "ISO-8859-13" -> ms.`ISO-8860-13`,
    "ISO-8859-15" -> ms.`ISO-8859-15`,
    "KOI8-R" -> ms.`KOI8-R`,
    "UTF-8" -> ms.`UTF-8`,
    "UTF-16" -> ms.`UTF-16`,
    "UTF-16BE" -> ms.`UTF-16BE`,
    "UTF-16LE" -> ms.`UTF-16LE`,
    "windows-1255" -> ms.`windows-1255`,
    "windows-1256" -> ms.`windows-1256`,
    "windows-1258" -> ms.`windows-1258`,
    "ISO-8859-3" -> ms.`ISO-8859-3`,
    "ISO-8859-6" -> ms.`ISO-8859-6`,
    "ISO-8859-8" -> ms.`ISO-8859-8`,
    "windows-31j" -> ms.`windows-31j`,
    "EUC-JP" -> ms.`EUC-JP`,
    "x-EUC-JP-LINUX" -> ms.`x-EUC-JP-LINUX`,
    "Shift_JIS" -> ms.Shift_JIS,
    "ISO-2022-JP" -> ms.`ISO-2022-JP`,
    "x-mswin-936" -> ms.`x-mswin-936`,
    "GB18030" -> ms.GB18030,
    "x-EUC-CN" -> ms.`x-EUC-CN`,
    "GBK" -> ms.GBK,
    "ISCII91" -> ms.ISCII91,
    "x-windows-949" -> ms.`x-windows-949`,
    "EUC-KR" -> ms.`EUC-KR`,
    "ISO-2022-KR" -> ms.`ISO-2022-KR`,
    "x-windows-950" -> ms.`x-windows-950`,
    "x-MS950-HKSCS" -> ms.`x-MS950-HKSCS`,
    "x-EUC-TW" -> ms.`x-EUC-TW`,
    "Big5" -> ms.Big5,
    "Big5-HKSCS" -> ms.`Big5-HKSCS`,
    "TIS-620" -> ms.`TIS-620`,
    "Big5_Solaris" -> ms.Big5_Solaris,
    "Cp037" -> ms.Cp037,
    "Cp273" -> ms.Cp273,
    "Cp277" -> ms.Cp277,
    "Cp278" -> ms.Cp278,
    "Cp280" -> ms.Cp280,
    "Cp284" -> ms.Cp284,
    "Cp285" -> ms.Cp285,
    "Cp297" -> ms.Cp297,
    "Cp420" -> ms.Cp420,
    "Cp424" -> ms.Cp424,
    "Cp437" -> ms.Cp437,
    "Cp500" -> ms.Cp500,
    "Cp737" -> ms.Cp737,
    "Cp775" -> ms.Cp775,
    "Cp838" -> ms.Cp838,
    "Cp850" -> ms.Cp850,
    "Cp852" -> ms.Cp852,
    "Cp855" -> ms.Cp855,
    "Cp856" -> ms.Cp856,
    "Cp857" -> ms.Cp857,
    "Cp858" -> ms.Cp858,
    "Cp860" -> ms.Cp860,
    "Cp861" -> ms.Cp861,
    "Cp862" -> ms.Cp862,
    "Cp863" -> ms.Cp863,
    "Cp864" -> ms.Cp864,
    "Cp865" -> ms.Cp865,
    "Cp866" -> ms.Cp866,
    "Cp868" -> ms.Cp868,
    "Cp869" -> ms.Cp869,
    "Cp870" -> ms.Cp870,
    "Cp871" -> ms.Cp871,
    "Cp874" -> ms.Cp874,
    "Cp875" -> ms.Cp875,
    "Cp918" -> ms.Cp918,
    "Cp921" -> ms.Cp921,
    "Cp922" -> ms.Cp922,
    "Cp930" -> ms.Cp930,
    "Cp933" -> ms.Cp933,
    "Cp935" -> ms.Cp935,
    "Cp937" -> ms.Cp937,
    "Cp939" -> ms.Cp939,
    "Cp942" -> ms.Cp942,
    "Cp942C" -> ms.Cp942C,
    "Cp943" -> ms.Cp943,
    "Cp943C" -> ms.Cp943C,
    "Cp948" -> ms.Cp948,
    "Cp949" -> ms.Cp949,
    "Cp949C" -> ms.Cp949C,
    "Cp950" -> ms.Cp950,
    "Cp964" -> ms.Cp964,
    "Cp970" -> ms.Cp970,
    "Cp1006" -> ms.Cp1006,
    "Cp1025" -> ms.Cp1025,
    "Cp1026" -> ms.Cp1026,
    "Cp1046" -> ms.Cp1046,
    "Cp1047" -> ms.Cp1047,
    "Cp1097" -> ms.Cp1097,
    "Cp1098" -> ms.Cp1098,
    "Cp1112" -> ms.Cp1112,
    "Cp1122" -> ms.Cp1122,
    "Cp1123" -> ms.Cp1123,
    "Cp1124" -> ms.Cp1124,
    "Cp1140" -> ms.Cp1140,
    "Cp1141" -> ms.Cp1141,
    "Cp1142" -> ms.Cp1142,
    "Cp1143" -> ms.Cp1143,
    "Cp1144" -> ms.Cp1144,
    "Cp1145" -> ms.Cp1145,
    "Cp1146" -> ms.Cp1146,
    "Cp1147" -> ms.Cp1147,
    "Cp1148" -> ms.Cp1148,
    "Cp1149" -> ms.Cp1149,
    "Cp1381" -> ms.Cp1381,
    "Cp1383" -> ms.Cp1383,
    "Cp33722" -> ms.Cp33722,
    "ISO2022_CN_CNS" -> ms.ISO2022_CN_CNS,
    "ISO2022_CN_GB" -> ms.ISO2022_CN_GB,
    "JISAutoDetect" -> ms.JISAutoDetect,
    "MS874" -> ms.MS874,
    "MacArabic" -> ms.MacArabic,
    "MacCentralEurope" -> ms.MacCentralEurope,
    "MacCroatian" -> ms.MacCroatian,
    "MacCyrillic" -> ms.MacCyrillic,
    "MacDingbat" -> ms.MacDingbat,
    "MacGreek" -> ms.MacGreek,
    "MacHebrew" -> ms.MacHebrew,
    "MacIceland" -> ms.MacIceland,
    "MacRoman" -> ms.MacRoman,
    "MacRomania" -> ms.MacRomania,
    "MacSymbol" -> ms.MacSymbol,
    "MacThai" -> ms.MacThai,
    "MacTurkish" -> ms.MacTurkish,
    "MacUkraine" -> ms.MacUkraine
    )

  msxml.documentUnstructured --> (
    ms.documentUnstructured > content
  )

  def annotationType = objectMap(msxml.annotationType, ms.annotationType,
    "alignment" -> ms.alignment,
    "discourseAnnotation" -> ms.discourseAnnotation,
    "discourseAnnotation-audienceReactions" -> ms.`discourseAnnotation-audienceReactions`,
    "discourseAnnotation-coreference" -> ms.`discourseAnnotation-coreference`,
    "discourseAnnotation-dialogueActs" -> ms.`discourseAnnotation-dialogueActs`,
    "discourseAnnotation-discourseRelations" -> ms.`discourseAnnotation-discourseRelations`,
    "lemmatization" -> ms.lemmatization,
    "morphosyntacticAnnotation-bPosTagging" -> ms.`morphosyntacticAnnotation-bPosTagging`,
    "morphosyntacticAnnotation-posTagging" -> ms.`morphosyntacticAnnotation-posTagging`,
    "segmentation" -> ms.segmentation,
    "semanticAnnotation" -> ms.semanticAnnotation,
    "semanticAnnotation-certaintyLevel" -> ms.`semanticAnnotation-certaintyLevel`,
    "semanticAnnotation-emotions" -> ms.`semanticAnnotation-emotions`,
    "semanticAnnotation-entityMentions" -> ms.`semanticAnnotation-entityMentions`,
    "semanticAnnotation-events" -> ms.`semanticAnnotation-events`,
    "semanticAnnotation-namedEntities" -> ms.`semanticAnnotation-namedEntities`,
    "semanticAnnotation-polarity" -> ms.`semanticAnnotation-polarity`,
    "semanticAnnotation-questionTopicalTarget" -> ms.`semanticAnnotation-questionTopicalTarget`,
    "semanticAnnotation-semanticClasses" -> ms.`semanticAnnotation-semanticClasses`,
    "semanticAnnotation-semanticRelations" -> ms.`semanticAnnotation-semanticRelations`,
    "semanticAnnotation-semanticRoles" -> ms.`semanticAnnotation-semanticRoles`,
    "semanticAnnotation-speechActs" -> ms.`semanticAnnotation-speechActs`,
    "semanticAnnotation-temporalExpressions" -> ms.`semanticAnnotation-temporalExpressions`,
    "semanticAnnotation-textualEntailment" -> ms.`semanticAnnotation-textualEntailment`,
    "semanticAnnotation-wordSenses" -> ms.`semanticAnnotation-wordSenses`,
    "speechAnnotation" -> ms.speechAnnotation,
    "speechAnnotation-orthographicTranscription" -> ms.`speechAnnotation-orthographicTranscription`,
    "speechAnnotation-paralanguageAnnotation" -> ms.`speechAnnotation-paralanguageAnnotation`,
    "speechAnnotation-phoneticTranscription" -> ms.`speechAnnotation-phoneticTranscription`,
    "speechAnnotation-prosodicAnnotation" -> ms.`speechAnnotation-prosodicAnnotation`,
    "speechAnnotation-soundEvents" -> ms.`speechAnnotation-soundEvents`,
    "speechAnnotation-soundToTextAlignment" -> ms.`speechAnnotation-soundToTextAlignment`,
    "speechAnnotation-speakerIdentification" -> ms.`speechAnnotation-speakerIdentification`,
    "speechAnnotation-speakerTurns" -> ms.`speechAnnotation-speakerTurns`,
    "speechAnnotation" -> ms.speechAnnotation,
    "stemming" -> ms.stemming,
    "structuralAnnotation" -> ms.structuralAnnotation,
    "syntacticAnnotation-shallowParsing" -> ms.`syntacticAnnotation-shallowParsing`,
    "syntacticAnnotation-subcategorizationFrames" -> ms.`syntacticAnnotation-subcategorizationFrames`,
    "syntacticAnnotation-treebanks" -> ms.`syntacticAnnotation-treebanks`,
    "syntacticosemanticAnnotation-links" -> ms.`syntacticosemanticAnnotation-links`,
    "translation" -> ms.translation,
    "transliteration" -> ms.transliteration,
    "discourseAnnotation-dialogueActs" -> ms.`discourseAnnotation-dialogueActs`,
    "modalityAnnotation-bodyMovements" -> ms.`modalityAnnotation-bodyMovements`,
    "modalityAnnotation-facialExpressions" -> ms.`modalityAnnotation-facialExpressions`,
    "modalityAnnotation-gazeEyeMovements" -> ms.`modalityAnnotation-gazeEyeMovements`,
    "modalityAnnotation-handArmGestures" -> ms.`modalityAnnotation-handArmGestures`,
    "modalityAnnotation-handManipulationOfObjects" -> ms.`modalityAnnotation-handManipulationOfObjects`,
    "modalityAnnotation-headMovements" -> ms.`modalityAnnotation-headMovements`,
    "modalityAnnotation-lipMovements" -> ms.`modalityAnnotation-lipMovements`,
    "semanticAnnotation-emotions" -> ms.`semanticAnnotation-emotions`,
    "other" -> ms.other
    )
	
  def useNLPSpecific = objectMap(msxml.useNLPSpecific, ms.useNLPSpecific,
    "parsing" -> ms.parsing,
    "contradictionDetection" -> ms.contradictionDetection,
    "opinionMining" -> ms.opinionMining,
    "wordSenseDisambiguation" -> ms.wordSenseDisambiguation,
    "voiceControl" -> ms.voiceControl,
    "topicDetection_Tracking" -> ms.topicDetection_Tracking,
    "textualEntailment" -> ms.textualEntailment,
    "textMining" -> ms.textMining,
    "textCategorisation" -> ms.textCategorisation,
    "terminologyExtraction" -> ms.terminologyExtraction,
    "summarisation" -> ms.summarisation,
    "spellChecking" -> ms.spellChecking,
    "speechUnderstanding" -> ms.speechUnderstanding,
    "speechToSpeechTranslation" -> ms.speechToSpeechTranslation,
    "speechSynthesis" -> ms.speechSynthesis,
    "speechRecognition" -> ms.speechRecognition,
    "signLanguageRecognition" -> ms.signLanguageRecognition,
    "signLanguageGeneration" -> ms.signLanguageGeneration,
    "semanticWeb" -> ms.semanticWeb,
    "questionAnswering" -> ms.questionAnswering,
    "informationExtraction" -> ms.informationExtraction,
    "posTagging" -> ms.posTagging,
    "personIdentification" -> ms.personIdentification,
    "naturalLanguageUnderstanding" -> ms.naturalLanguageUnderstanding,
    "naturalLanguageGeneration" -> ms.naturalLanguageGeneration,
    "namedEntityRecognition" -> ms.namedEntityRecognition,
    "multimediaDocumentProcessing" -> ms.multimediaDocumentProcessing,
    "morphosyntacticTagging" -> ms.morphosyntacticTagging,
    "morphologicalAnalysis" -> ms.morphologicalAnalysis,
    "linguisticResearch" -> ms.linguisticResearch,
    "lexiconEnhancement" -> ms.lexiconEnhancement,
    "lemmatization" -> ms.lemmatization,
    "languageModelsTraining" -> ms.languageModelsTraining,
    "languageModelling" -> ms.languageModelling,
    "languageIdentification" -> ms.languageIdentification,
    "knowledgeRepresentation" -> ms.knowledgeRepresentation,
    "knowledgeDiscovery" -> ms.knowledgeDiscovery,
    "emotionRecognition" -> ms.emotionRecognition,
    "emotionGeneration" -> ms.emotionGeneration,
    "documentClassification" -> ms.documentClassification,
    "derivationalMorphologicalAnalysis" -> ms.derivationalMorphologicalAnalysis,
    "coreferenceResolution" -> ms.coreferenceResolution,
    "bilingualLexiconInduction" -> ms.bilingualLexiconInduction,
    "annotation" -> ms.annotation,
    "webServices" -> ms.webServices,
    "eventExtraction" -> ms.eventExtraction,
    "semanticRoleLabelling" -> ms.semanticRoleLabelling,
    "readingAndWritingAidApplications" -> ms.readingAndWritingAidApplications,
    "temporalExpressionRecognition" -> ms.temporalExpressionRecognition,
    "intra-documentCoreferenceResolution" -> ms.`intra-documentCoreferenceResolution`,
    "visualSceneUnderstanding" -> ms.visualSceneUnderstanding,
    "entityMentionRecognition" -> ms.entityMentionRecognition,
    "sentimentAnalysis" -> ms.sentimentAnalysis,
    "machineTranslation" -> ms.machineTranslation,
    "persuasiveExpressionMining" -> ms.persuasiveExpressionMining,
    "qualitativeAnalysis" -> ms.qualitativeAnalysis,
    "texToSpeechSynthesis" -> ms.texToSpeechSynthesis,
    "personRecognition" -> ms.personRecognition,
    "textGeneration" -> ms.textGeneration,
    "avatarSynthesis" -> ms.avatarSynthesis,
    "discourseAnalysis" -> ms.discourseAnalysis,
    "expressionRecognition" -> ms.expressionRecognition,
    "faceRecognition" -> ms.faceRecognition,
    "faceVerification" -> ms.faceVerification,
    "humanoidAgentSynthesis" -> ms.humanoidAgentSynthesis,
    "informationRetrieval" -> ms.informationRetrieval,
    "lexiconAccess" -> ms.lexiconAccess,
    "lexiconAcquisitionFromCorpora" -> ms.lexiconAcquisitionFromCorpora,
    "lexiconExtractionFromLexica" -> ms.lexiconExtractionFromLexica,
    "lexiconFormatConversion" -> ms.lexiconFormatConversion,
    "lexiconMerging" -> ms.lexiconMerging,
    "lexiconVisualization" -> ms.lexiconVisualization,
    "lipTrackingAnalysis" -> ms.lipTrackingAnalysis,
    "multimediaDevelopment" -> ms.multimediaDevelopment,
    "speakerIdentification" -> ms.speakerIdentification,
    "speakerVerification" -> ms.speakerVerification,
    "speechLipsCorrelationAnalysis" -> ms.speechLipsCorrelationAnalysis,
    "speechAnalysis" -> ms.speechAnalysis,
    "speechAssistedVideoControl" -> ms.speechAssistedVideoControl,
    "speechVerification" -> ms.speechVerification,
    "spokenDialogueSystems" -> ms.spokenDialogueSystems,
    "talkingHeadSynthesis" -> ms.talkingHeadSynthesis,
    "userAuthentication" -> ms.userAuthentication,
    "other" -> ms.other
    )

  def conformanceToClassificationScheme = objectMap(msxml.conformanceToClassificationScheme, ms.conformanceToClassificationScheme,
    "ANC_domainClassification" -> ms.ANC_domainClassification,
    "ANC_genreClassification" -> ms.ANC_genreClassification,
    "BNC_domainClassification" -> ms.BNC_domainClassification,
    "BNC_textTypeClassification" -> ms.BNC_textTypeClassification,
    "DDC_classification" -> ms.DDC_classification,
    "libraryOfCongress_domainClassification" -> ms.libraryOfCongress_domainClassification,
    "libraryofCongressSubjectHeadings_classification" -> ms.libraryofCongressSubjectHeadings_classification,
    "MeSH_classification" -> ms.MeSH_classification,
    "NLK_classification" -> ms.NLK_classification,
    "PAROLE_topicClassification" -> ms.PAROLE_topicClassification,
    "PAROLE_genreClassification" -> ms.PAROLE_genreClassification,
    "UDC_classification" -> ms.UDC_classification,
    "other" -> ms.other)
	
  def segmentationLevel = objectMap(msxml.segmentationLevel, ms.segmentationLevel,
    "paragraph" -> ms.paragraph,
    "sentence" -> ms.sentence,
    "clause" -> ms.clause,
    "word" -> ms.word,
    "wordGroup" -> ms.wordGroup,
    "utterance" -> ms.utterance,
    "topic" -> ms.topic,
    "signal" -> ms.signal,
    "phoneme" -> ms.phoneme,
    "syllable" -> ms.syllable,
    "phrase" -> ms.phrase,
    "diphone" -> ms.diphone,
    "prosodicBoundaries" -> ms.prosodicBoundaries,
    "frame" -> ms.frame,
    "scene" -> ms.scene,
    "shot" -> ms.shot,
    "other" -> ms.other)

  def tagset = stringMap(msxml.tagset,ms.tagset)

  def annotationFormat = stringMap(msxml.annotationFormat, ms.annotationFormat)

  def mimeType = stringMap(msxml.mimeType, ms.mimeType)

  msxml.sizeInfo --> (
    ms.sizeInfo > node(frag("sizeInfo")) (
      sizeInfoType
    )
  )

  // Derived from META-SHARE-LanguageMetadata.xsd
  
  msxml.characterEncodingInfo --> (
    ms.characterEncodingInfo > node(frag("characterEncodingInfo")) (
      rdf_type > ms.CharacterEncodingInfo,
      characterEncoding,
      handle(msxml.sizePerCharacterEncoding)
    )
  )

  msxml.sizePerCharacterEncoding --> (
    ms.sizePerCharacterEncoding > node(frag("sizePerCharacterEncoding")) (
      sizeInfoType
    )
  )

  msxml.timeCoverageInfo --> (
    ms.timeCoverageInfo > node(frag("timeCoverageInfo")) (
      rdf_type > ms.TimeCoverageInfo,
      stringMap(msxml.timeCoverage, ms.timeCoverage),
      handle(msxml.sizePerTimeCoverage)
    )
  )

  msxml.sizePerTimeCoverage --> (
    ms.sizePerTimeCoverage > node(frag("sizePerTimeCoverage")) (
      sizeInfoType
    )
  )

  msxml.geographicCoverageInfo --> (
    ms.geographicCoverageInfo > node(frag("geographicCoverageInfo")) (
      rdf_type > ms.GeographicCoverageInfo,
      stringMap(msxml.geographicCoverage, ms.geographicCoverage),
      handle(msxml.sizePerGeographicCoverage)
    )
  )

  msxml.sizePerGeographicCoverage --> (
    ms.sizePerGeographicCoverage > node(frag("sizePerGeographicCoverage")) (
      sizeInfoType
    )
  )

  msxml.lingualityInfo --> (
    objectMap(msxml.lingualityType, ms.lingualityType,
      "monolingual" -> ms.monolingual,
      "bilingual" -> ms.bilingual,
      "multilingual" -> ms.multilingual
      ),
    objectMap(msxml.multilingualityType, ms.multilingualityType,
      "parallel" -> ms.parallel,
      "comparable" -> ms.comparable,
      "multilingualSingleText" -> ms.multilingualSingleText,
      "other" -> ms.other
      ),
    stringMap(msxml.multilingualityTypeDetails, ms.multilingualityTypeDetails)
  )

  msxml.languageVarietyInfo --> (
    ms.languageVarietyInfo > node(frag("languageVarietyInfo")) (
      rdf_type > ms.LanguageVarietyInfo,
      objectMap(msxml.languageVarietyType, ms.languageVarietyType,
        "dialect" -> ms.dialect,
        "jargon" -> ms.jargon,
        "other" -> ms.other
        ),
      languageVarietyName,
      handle(msxml.sizePerLanguageVariety)
    )
  )

  msxml.sizePerLanguageVariety --> (
    ms.sizePerLanguageVariety > node(frag("sizePerLanguageVariety")) (
      sizeInfoType
    )
  )

  def languageName = stringMap(msxml.languageName, ms.languageName)

  def languageId = stringMap(msxml.languageId, dc.language)

  msxml.languageInfo --> (
    ms.languageInfo > node(frag("languageInfo")) (
      rdf_type > ms.LanguageInfo,
      languageId,
      languageName,
      stringMap(msxml.languageScript, ms.languageScript),
      handle(msxml.sizePerLanguage),
      handle(msxml.languageVarietyInfo)
    )
  )

  msxml.sizePerLanguage --> (
    ms.sizePerLanguage > node(frag("sizePerLanguage")) (
      sizeInfoType
    )
  )

  def languageVarietyName = stringMap(msxml.languageVarietyName, ms.languageVarietyName)

  // Derived from META-SHARE-LicenseMetadata.xsd
  
  msxml.distributionInfo --> (
    dcat.distribution > node(frag("distribution")) (
      objectMap(msxml.availability, ms.availability,
        "available-unrestrictedUse" -> ms.`available-unrestrictedUse`,
        "available-restrictedUse" -> ms.`available-restrictedUse`,
        "notAvailableThroughMetaShare" -> ms.availableThroughOtherDistributors,
        "underNegotiation" -> ms.underNegotiation
        ),
      handle(msxml.licenceInfo),
      handle(msxml.iprHolder),
      dateMap(msxml.availabilityEndDate, ms.availabilityEndDate),
      dateMap(msxml.availabilityStartDate, ms.availabilityStartDate)
    )
  )

  msxml.membershipInfo --> (
    ms.membershipInfo > node(frag("membershipInfo")) (
      rdf_type > ms.MembershipInfo,
      boolMap(msxml.member, ms.member),
      objectMap(msxml.membershipInstitution, ms.membershipInstitution,
        "ELRA" -> ms.ELRA,
        "LDC" -> ms.LDC,
        "TST-CENTRALE" -> ms.`TST-CENTRALE`,
        "other" -> ms.other
        )
    )
  )

  msxml.iprHolder --> (
    ms.iprHolder > node(frag("iprHolder")) (
      actorInfoType
    )
  )

  // Yes Metashare does spell it licence!
  msxml.licenceInfo --> (
    linkMap(msxml.downloadLocation, dcat.downloadURL),
    dct.license > node(frag("licenceInfo")) (
      rdf_type > ms.LicenceInfo,
      objectMap(msxml.licence, owl.sameAs,
        "CC-BY" -> uri("https://creativecommons.org/licenses/by/4.0/"),
        "CC-BY-NC" -> uri("https://creativecommons.org/licenses/by-nc/4.0/"),
        "CC-BY-NC-ND" -> uri("https://creativecommons.org/licenses/by-nc-nd/4.0/"),
        "CC-BY-NC-SA" -> uri("https://creativecommons.org/licenses/by-nc-sa/4.0/"),
        "CC-BY-ND" -> uri("https://creativecommons.org/licenses/by-nd/4.0/"),
        "CC-BY-SA" -> uri("https://creativecommons.org/licenses/by-sa/4.0/"),
        "CC-ZERO" -> uri("http://creativecommons.org/publicdomain/zero/1.0/"),
        "MS-C-NoReD" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20Commercial%20%20NoRedistribution_v0%207.pdf"),
        "MS-C-NoReD-FF" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20Commercial%20NoRedistribution%20For-a-Fee_v0%207.pdf"),
        "MS-C-NoReD-ND" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20Commercial%20NoRedistribution%20NoDerivatives-v1.0.pdf"),
        "MS-C-NoReD-ND-FF" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20Commercial%20NoRedistribution%20NoDerivatives%20For-a-fee-v1.0.pdf"),
        "MS-NC-NoReD" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20NonCommercial%20NoRedistribution-v%201.0.pdf"),
        "MS-NC-NoReD-FF" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20NonCommercial%20NoRedistribution%20For-a-Fee-v%201.0.pdf"),
        "MS-NC-NoReD-ND" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20NonCommercial%20NoRedistribution%20NoDerivatives-v1.0.pdf"),
        "MS-NC-NoReD-ND-FF" ->
        uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20NonCommercial%20NoRedistribution%20NoDerivatives%20For-a-fee-v%201.0.pdf"),
        "MSCommons-BY" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BY%20v1.0.pdf"),
        "MSCommons-BY-NC" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BYNC%20v1.0.pdf"),
        "MSCommons-BY-NC-ND" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BYNCND%20v1.0.pdf"),
        "MSCommons-BY-NC-SA" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BYNCSA%20v1.0.pdf"),
        "MSCommons-BY-ND" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BYND%20v1.0.pdf"),
        "MSCommons-BY-SA" -> uri("http://www.meta-net.eu/meta-share/meta-share-licenses/META-SHARE%20COMMONS_BYSA%20v1.0.pdf"),
        "CLARIN_ACA" -> uri("https://kitwiki.csc.fi/twiki/pub/FinCLARIN/ClarinEULA/CLARIN-EULA-ACA-2014-10.rtf"),
        "CLARIN_ACA-NC" -> uri("https://kitwiki.csc.fi/twiki/pub/FinCLARIN/ClarinEULA/CLARIN-EULA-ACA-2014-10.rtf"),
        "CLARIN_PUB" -> uri("https://kitwiki.csc.fi/twiki/pub/FinCLARIN/ClarinEULA/CLARIN-EULA-PUB-2014-10.rtf"),
        "CLARIN_RES" -> uri("https://kitwiki.csc.fi/twiki/pub/FinCLARIN/ClarinEULA/CLARIN-EULA-RES-2014-10.rtf"),
        "ELRA_END_USER" -> uri("http://www.elra.info/IMG/pdf_ENDUSER_140312.pdf"),
        "ELRA_EVALUATION" -> uri("http://www.elra.info/IMG/pdf_EVALUATION_140312.pdf"),
        "ELRA_VAR" -> uri("http://www.elra.info/IMG/pdf_VAR_140312.pdf"),
        "AGPL" -> uri("https://gnu.org/licenses/agpl.html"),
        "ApacheLicence_2.0" -> uri("http://www.apache.org/licenses/LICENSE-2.0.html"),
        "BSD" -> uri("http://opensource.org/licenses/BSD-3-Clause"),
        "BSD-style" -> uri("http://opensource.org/licenses/BSD-2-Clause"),
        "GFDL" -> uri("http://www.gnu.org/copyleft/fdl.html"),
        "GPL" -> uri("http://www.gnu.org/copyleft/gpl.html"),
        "LGPL" -> uri("https://www.gnu.org/licenses/lgpl.html"),
        "Princeton_Wordnet" -> uri("http://wordnet.princeton.edu/wordnet/license/"),
        "proprietary" -> ms.`proprietary`,
        "underNegotiation" -> ms.`underNegotiation`,
        "other" -> ms.`other`
        ),
      //handle(msxml.restrictionsOfUse),
      objectMap(msxml.restrictionsOfUse, ms.conditionsOfUse,
        "informLicensor" -> ms.informLicensor,
        "redeposit" -> ms.redeposit,
        "onlyMSmembers" -> ms.onlyMSmembers,
        "academic-nonCommercialUse" -> ms.`academic-nonCommercialUse`,
        "evaluationUse" -> ms.evaluationUse,
        "commercialUse" -> ms.commercialUse,
        "attribution" -> ms.attribution,
        "shareAlike" -> ms.shareAlike,
        "noDerivatives" -> ms.noDerivatives,
        "noRedistribution" -> ms.noRedistribution,
        "other" -> ms.other
      ),
      dataMap(msxml.distributionAccessMedium, odrl.deliveryChannel,
        "webExecutable" -> "Web Executable",
        "paperCopy" -> "Paper Copy",
        "hardDisk" -> "Hard Disk",
        "bluRay" -> "BluRay",
        "DVD-R" -> "DVD-R",
        "CD-ROM" -> "CD-ROM",
        "downloadable" -> "Downloadable",
        "accessibleThroughInterface" -> "Accessible Through Interface",
        "other" -> "Other"
        ),
      linkMap(msxml.executionLocation, ms.executionLocation),
      stringMap(msxml.fee, ms.fee),
      langStringMap(msxml.attributionText, ms.attributionText),
      handle(msxml.licensor),
      handle(msxml.distributionRightsHolder),
      objectMap(msxml.userNature, ms.userNature,
        "academic" -> ms.academic,
        "commercial" -> ms.commercial
        ),
      handle(msxml.membershipInfo)
    )
  )

//  msxml.restrictionsOfUse.when(content === "informLicensor") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.duty > node(frag("duty")) (
//        rdf_type > odrl.Duty,
//        odrl.action > cc.Notify
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "redeposit") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.duty > node(frag("duty")) (
//        rdf_type > odrl.Duty,
//        odrl.action > ms.redeposit
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "onlyMSmembers") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.constraint > node(frag("constraint")) (
//        rdf_type > odrl.Constraint,
//        odrl.operator > (odrl + "eq"),
//        // Is this the right property??
//        odrl.recipient > ms.onlyMSmembers
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "academic-nonCommercialUse") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.constraint > node(frag("constraint")) (
//        rdf_type > odrl.Constraint,
//        odrl.operator > (odrl + "eq"),
//        odrl.purpose > ms.academicUse
//      )
//    ),
//    odrl.prohibition > node(frag("prohibition")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Prohibition,
//      odrl.action > cc.CommercialUse,
//      odrl.action > cc.Distribution
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "evaluationUse") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.constraint > node(frag("constraint")) (
//        rdf_type > odrl.Constraint
//        // What are the triples here?
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "commercialUse") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      // Is this correct?
//      odrl.action > cc.CommercialUse
//    )
//  )
//  
//  msxml.restrictionsOfUse.when(content === "attribution") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.duty > node(frag("duty")) (
//        rdf_type > odrl.Duty,
//        odrl.action > cc.Attribution
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "shareAlike") --> (
//    odrl.permission > node(frag("permission")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Permission,
//      odrl.duty > node(frag("duty")) (
//        rdf_type > odrl.Duty,
//        odrl.action > cc.ShareALike
//      )
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "noDerivatives") --> (
//    odrl.prohibition > node(frag("prohibition")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Prohibition,
//      odrl.action > cc.DerivativeWorks
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "noRedistribution") --> (
//    odrl.prohibition > node(frag("prohibition")) (
//      rdf_type > ms.ConditionsOfUse,
//      rdf_type > odrl.Prohibition,
//      odrl.action > cc.Reproduction
//    )
//  )
//
//  msxml.restrictionsOfUse.when(content === "other") --> (
//  )

  msxml.licensor --> (
    ms.licensor > node(frag("licensor")) (
      actorInfoType
    )
  )

  msxml.distributionRightsHolder --> (
    ms.distributionRightsHolder > node(frag("distributionRightsHolder")) (
      actorInfoType
    )
  )
      
  // Derived from META-SHARE-RoleTypes.xsd

  msxml.communicationInfo --> (
    stringMap(msxml.email, ms.email),
    linkMap(msxml.url, ms.url),
    stringMap(msxml.address, ms.address),
    stringMap(msxml.zipCode, ms.zipCode),
    stringMap(msxml.city, ms.city),
    stringMap(msxml.region, ms.region),
    stringMap(msxml.country, ms.country),
    stringMap(msxml.telephoneNumber, ms.telephoneNumber),
    stringMap(msxml.faxNumber, ms.faxNumber)
  )

  msxml.organizationInfo --> (
    ms.organizationInfo > node(frag("organizationInfo")) (
      organizationInfoType
    )
  )
  
  def organizationInfoType = (
      (rdf_type > ms.Organization) ++
      langStringMap(msxml.organizationName, ms.organizationName) ++
      langStringMap(msxml.organizationShortName, ms.organizationShortName) ++
      langStringMap(msxml.departmentName, ms.departmentName) ++
      handle(msxml.communicationInfo)
    )

  msxml.personList --> (
    ms.personList > node(frag("personList")) (
      rdf_type > ms.PersonList,
      handle(msxml.personInfo)
    )
  )

  msxml.organizationList --> (
    ms.organizationList > node(frag("organizationList")) (
      rdf_type > ms.OrganizationList,
      handle(msxml.organizationInfo)
    )
  )

  def personInfoType = 
      (rdf_type > ms.Person) ++
      (rdf_type > foaf.Person) ++
      langStringMap(msxml.surname, foaf.surname) ++
      langStringMap(msxml.givenName, foaf.givenName) ++
      stringMap(msxml.sex, foaf.gender) ++
      handle(msxml.communicationInfo) ++
      stringMap(msxml.position, ms.position) ++
      handle(msxml.affiliation)
 
  msxml.personInfo --> (
    ms.personInfo > node(frag("personInfo")) (
      personInfoType
   )
  )

  msxml.affiliation --> (
    ms.affiliation > node(frag("affiliation")) (
      organizationInfoType
    )
  )

  def actorInfoType = (
    (rdf_type > ms.Actor) ++ 
    handle(msxml.personInfo) ++
    handle(msxml.organizationInfo)
  )

  // Derived from META-SHARE-UsageMetadata.xsd
  
  def projectInfoType = 
    (rdf_type > ms.Project) ++
    langStringMap(msxml.projectName, ms.projectName) ++
    langStringMap(msxml.projectShortName, ms.projectShortName) ++
    stringMap(msxml.projectID, ms.projectID) ++
    linkMap(msxml.url, ms.url) ++
    objectMap(msxml.fundingType, ms.fundingType,
      "other" -> ms.other,
      "ownFunds" -> ms.ownFunds,
      "nationalFunds" -> ms.nationalFunds,
      "euFunds" -> ms.euFunds
      ) ++
    stringMap(msxml.funder, ms.funder) ++
    stringMap(msxml.fundingCountry, ms.fundingCountry) ++
    dateMap(msxml.projectStartDate, ms.projectStartDate) ++
    dateMap(msxml.projectEndDate, ms.projectEndDate)

  msxml.usageInfo --> (
    handle(msxml.accessTool),
    handle(msxml.resourceAssociatedWith),
    handle(msxml.foreseenUseInfo),
    handle(msxml.actualUseInfo)
  )

  msxml.accessTool --> (
    ms.accessTool > targetResourceInfoType
  )

  msxml.resourceAssociatedWith --> (
    ms.resourceAssociatedWith > targetResourceInfoType
  )

  msxml.foreseenUseInfo --> (
    ms.foreseenUseInfo > node(frag("foreseenUseInfo")) (
      rdf_type > ms.ForeseenUseInfo,
      objectMap(msxml.foreseenUse, ms.use,
        "humanUse" -> ms.humanUse,
        "nlpApplications" -> ms.nlpApplications
        ),
      useNLPSpecific
    )
  )

  msxml.actualUseInfo --> (
    ms.actualUseInfo > node(frag("actualUseInfo")) (
      rdf_type > ms.ActualUseInfo,
      objectMap(msxml.actualUse, ms.use,
        "humanUse" -> ms.humanUse,
        "nlpApplications" -> ms.nlpApplications
        ),
      useNLPSpecific,
      handle(msxml.usageReport),
      handle(msxml.derivedResource),
      handle(msxml.usageProject),
      stringMap(msxml.actualUseDetails, ms.actualUseDetails)
    )
  )

  msxml.usageReport --> (
    ms.usageReport > node(frag("usageReport")) (
      documentationInfoType
    )
  )

  msxml.derivedResource --> (
    ms.derivedResource > targetResourceInfoType
  )

  msxml.usageProject--> (
    ms.usageProject > node(frag("usageProject")) (
      projectInfoType
    )
  )

  msxml.projectInfoList --> (
    handle(msxml.projectInfo)
  )

  msxml.projectInfoList --> (
    ms.projectInfo > node(frag("projectInfo")) (
      projectInfoType
    )
  )

  // Derived from resourceTypes/Corpus.xsd

  msxml.corpusInfo --> (
    dct.`type` > ms.corpus,
    ms.corpusInfo > node(frag("corpusInfo")) (
      rdf_type > ms.CorpusInfo,
      ms.resourceType > ms.corpus,
      handle(msxml.corpusMediaType)
    )
  )

  msxml.corpusMediaType --> (
    handle(msxml.corpusTextInfo),
    handle(msxml.corpusAudioInfo),
    handle(msxml.corpusVideoInfo),
    handle(msxml.corpusImageInfo),
    handle(msxml.corpusTextNumericalInfo),
    handle(msxml.corpusTextNgramInfo)
  )

  // Derived from resourceTypes/languageDescription.xsd

  msxml.relatedLexiconInfo --> (
    objectMap(msxml.relatedLexiconType, ms.relatedLexiconType,
      "included" -> ms.included,
      "attached" -> ms.attached,
      "compatible" -> ms.compatible,
      "none" -> ms.none
      ),
    stringMap(msxml.attachedLexiconPosition, ms.attachedLexiconPosition),
    objectMap(msxml.compatibleLexiconType, ms.compatibleLexiconType,
        "wordList" -> ms.wordlist,
        "computationalLexicon" -> ms.computationalLexicon,
        "ontology" -> ms.ontology,
        "wordnet" -> ms.wordnet,
        "thesaurus" -> ms.thesaurus,
        "framenet" -> ms.framenet,
        "terminologicalResource" -> ms.terminologicalResource,
        "machineReadableDictionary" -> ms.machineReadableDictionary,
        "lexicon" -> ms.lexicon,
        "wordlist" -> ms.wordlist,
        "morphologicalLexicon" -> ms.morphologicalLexicon,
        "other" -> ms.other
      )
  )

  msxml.languageDescriptionInfo --> (
    dct.`type` > ms.languageDescription,
    ms.languageDescriptionInfo > node(frag("languageDescriptionInfo")) (
      rdf_type > ms.LanguageDescriptionInfo,
      ms.resourceType > ms.languageDescription,
      objectMap(msxml.languageDescriptionType, ms.languageDescriptionType,
        "grammar" -> ms.grammar,
        "other" -> ms.other
        ),
      handle(msxml.languageDescriptionEncodingInfo),
      handle(msxml.languageDescriptionOperationInfo),
      handle(msxml.languageDescriptionPerformanceInfo),
      handle(msxml.creationInfo),
      handle(msxml.languageDescriptionMediaType)
    )
  )

  msxml.languageDescriptionMediaType --> (
    handle(msxml.languageDescriptionTextInfo),
    handle(msxml.languageDescriptionVideoInfo),
    handle(msxml.languageDescriptionImageInfo)
  )

  msxml.languageDescriptionEncodingInfo --> (
    objectMap(msxml.encodingLevel, ms.encodingLevel,
      "phonetics" -> ms.phonetics,
      "phonology" -> ms.phonology,
      "semantics" -> ms.semantics,
      "morphology" -> ms.morphology,
      "syntax" -> ms.syntax,
      "pragmatics" -> ms.pragmatics,
      "other" -> ms.other
      ),
    handle(msxml.conformanceToStandardsBestPractices),
    stringMap(msxml.theoreticModel, ms.theoreticModel),
    handle(msxml.formalism),
    objectMap(msxml.task, ms.task,
      "anaphoraResolution" -> ms.anaphoraResolution,
      "chunking" -> ms.chunking,
      "parsing" -> ms.parsing,
      "npRecognition" -> ms.npRecognition,
      "titlesParsing" -> ms.titlesParsing,
      "definitionsParsing" -> ms.definitionsParsing,
      "analysis" -> ms.analysis,
      "generation" -> ms.generation,
      "other" -> ms.other
      ),
    objectMap(msxml.grammaticalPhenomenaCoverage, ms.grammaticalPhenomenaCoverage,
      "clauseStructure" -> ms.clauseStructure,
      "ppAttachment" -> ms.ppAttachment,
      "npStructure" -> ms.npStructure,
      "coordination" -> ms.coordination,
      "anaphora" -> ms.anaphora,
      "other" -> ms.other
      ),
    boolMap(msxml.weightedGrammar, ms.weightedGrammar)
  )

  msxml.languageDescriptionOperationInfo --> (
    handle(msxml.runningEnvironmentInfo),
    handle(msxml.relatedLexiconInfo)
  )

  msxml.languageDescriptionPerformanceInfo --> (
    stringMap(msxml.robustness, ms.robustness),
    stringMap(msxml.shallowness, ms.shallowness),
    stringMap(msxml.output, ms.output)
  )

  // Derived from resourceTypes/lexicalConceptualResources.xsd

  msxml.lexicalConceptualResourceInfo --> (
    dct.`type` > ms.lexicalConceptualResource,
    ms.lexicalConceptualResourceInfo > node(frag("lexicalConceptualResourceInfo")) (
      rdf_type > ms.LexicalConceptualResourceInfo,
      handle(msxml.resourceType),
      objectMap(msxml.lexicalConceptualResourceType, ms.lexicalConceptualResourceType,
        "wordList" -> ms.wordList,
        "computationalLexicon" -> ms.computationalLexicon,
        "morphologicalLexicon" -> ms.morphologicalLexicon,
        "ontology" -> ms.ontology,
        "wordnet" -> ms.wordnet,
        "thesaurus" -> ms.thesaurus,
        "framenet" -> ms.framenet,
        "terminologicalResource" -> ms.terminologicalResource,
        "machineReadableDictionary" -> ms.machineReadableDictionary,
        "lexicon" -> ms.lexicon,
        "other" -> ms.other
        ),
      handle(msxml.lexicalConceptualResourceEncodingInfo),
      handle(msxml.creationInfo),
      handle(msxml.lexicalConceptualResourceMediaType)
    )
  )

  msxml.lexicalConceptualResourceMediaType --> (
    handle(msxml.lexicalConceptualResourceTextInfo),
    handle(msxml.lexicalConceptualResourceAudioInfo),
    handle(msxml.lexicalConceptualResourceVideoInfo),
    handle(msxml.lexicalConceptualResourceImageInfo)
  )

  msxml.lexicalConceptualResourceEncodingInfo --> (
    objectMap(msxml.encodingLevel, ms.encodingLevel,
      "phonetics" -> ms.phonetics,
      "phonology" -> ms.phonology,
      "semantics" -> ms.semantics,
      "morphology" -> ms.morphology,
      "syntax" -> ms.syntax,
      "pragmatics" -> ms.pragmatics,
      "other" -> ms.other
      ),
    objectMap(msxml.linguisticInformation, ms.linguisticInformation,
      "accentuation" -> ms.`accentuation`,
      "lemma" -> ms.`lemma`,
      "lemma-MultiWordUnits" -> ms.`lemma-MultiWordUnits`,
      "lemma-Variants" -> ms.`lemma-Variants`,
      "lemma-Abbreviations" -> ms.`lemma-Abbreviations`,
      "lemma-Compounds" -> ms.`lemma-Compounds`,
      "lemma-CliticForms" -> ms.`lemma-CliticForms`,
      "partOfSpeech" -> ms.`partOfSpeech`,
      "morpho-Case" -> ms.`morpho-Case`,
      "morpho-Gender" -> ms.`morpho-Gender`,
      "morpho-Number" -> ms.`morpho-Number`,
      "morpho-Degree" -> ms.`morpho-Degree`,
      "morpho-IrregularForms" -> ms.`morpho-IrregularForms`,
      "morpho-Mood" -> ms.`morpho-Mood`,
      "morpho-Tense" -> ms.`morpho-Tense`,
      "morpho-Person" -> ms.`morpho-Person`,
      "morpho-Aspect" -> ms.`morpho-Aspect`,
      "morpho-Voice" -> ms.`morpho-Voice`,
      "morpho-Auxiliary" -> ms.`morpho-Auxiliary`,
      "morpho-Inflection" -> ms.`morpho-Inflection`,
      "morpho-Reflexivity" -> ms.`morpho-Reflexivity`,
      "syntax-SubcatFrame" -> ms.`syntax-SubcatFrame`,
      "semantics-Traits" -> ms.`semantics-Traits`,
      "semantics-SemanticClass" -> ms.`semantics-SemanticClass`,
      "semantics-CrossReferences" -> ms.`semantics-CrossReferences`,
      "semantics-Relations" -> ms.`semantics-Relations`,
      "semantics-Relations-Hyponyms" -> ms.`semantics-Relations-Hyponyms`,
      "semantics-Relations-Hyperonyms" -> ms.`semantics-Relations-Hyperonyms`,
      "semantics-Relations-Synonyms" -> ms.`semantics-Relations-Synonyms`,
      "semantics-Relations-Antonyms" -> ms.`semantics-Relations-Antonyms`,
      "semantics-Relations-Troponyms" -> ms.`semantics-Relations-Troponyms`,
      "semantics-Relations-Meronyms" -> ms.`semantics-Relations-Meronyms`,
      "usage-Frequency" -> ms.`usage-Frequency`,
      "usage-Register" -> ms.`usage-Register`,
      "usage-Collocations" -> ms.`usage-Collocations`,
      "usage-Examples" -> ms.`usage-Examples`,
      "usage-Notes" -> ms.`usage-Notes`,
      "definition/gloss" -> ms.`definition-gloss`,
      "translationEquivalent" -> ms.`translationEquivalent`,
      "phonetics-Transcription" -> ms.`phonetics-Transcription`,
      "semantics-Domain" -> ms.`semantics-Domain`,
      "semantics-EventType" -> ms.`semantics-EventType`,
      "semantics-SemanticRoles" -> ms.`semantics-SemanticRoles`,
      "statisticalProperties" -> ms.`statisticalProperties`,
      "morpho-Derivation" -> ms.`morpho-Derivation`,
      "semantics-QualiaStructure" -> ms.`semantics-QualiaStructure`,
      "syntacticoSemanticLinks" -> ms.`syntacticoSemanticLinks`,
      "other" -> ms.other
      ),
    handle(msxml.conformanceToStandardsBestPractices),
    stringMap(msxml.theoreticModel, ms.theoreticModel),
    stringMap(msxml.externalRef, ms.externalRef),
    objectMap(msxml.extratextualInformation, ms.extratextualInformation,
      "images" -> ms.images,
      "videos" -> ms.videos,
      "soundRecordings" -> ms.soundRecordings,
      "other" -> ms.other
      ),
    objectMap(msxml.extraTextualInformationUnit, ms.extraTextualInformationUnit,
      "word" -> ms.word,
      "lemma" -> ms.lemma,
      "semantics" -> ms.semantics,
      "example" -> ms.example,
      "syntax" -> ms.syntax,
      "lexicalUnit" -> ms.lexicalUnit,
      "other" -> ms.other
    )
  )

  // Derived from resourceTypes/toolService.xsd
	
  msxml.toolServiceInfo --> (
    dct.`type` > ms.toolService,
    ms.toolServiceInfo > node(frag("toolServiceInfo")) (
      rdf_type > ms.ToolServiceInfo,
      ms.resourceType > ms.toolService,
      objectMap(msxml.toolServiceType, ms.toolServiceType,
        "tool" -> ms.tool,
        "service" -> ms.service,
        "platform" -> ms.platform,
        "suiteOfTools" -> ms.suiteOfTools,
        "infrastructure" -> ms.infrastructure,
        "architecture" -> ms.architecture,
        "nlpDevelopmentEnvironment" -> ms.nlpDevelopmentEnvironment,
        "other" -> ms.other
        ),
      stringMap(msxml.toolServiceSubtype, ms.toolServiceSubtype),
      boolMap(msxml.languageDependent, ms.languageDependent),
      handle(msxml.inputInfo),
      handle(msxml.outputInfo),
      handle(msxml.toolServiceOperationInfo),
      handle(msxml.toolServiceEvaluationInfo),
      handle(msxml.toolServiceCreationInfo)
    )
  )
  
  msxml.inputInfo --> (
    ms.inputInfo > node(frag("inputInfo")) (
      rdf_type > ms.InputInfo,
      mediaType,
      objectMap(msxml.resourceType, ms.resourceType,
        "corpus" -> ms.corpus,
        "lexicalConceptualResource" -> ms.lexicalConceptualResource,
        "languageDescription" -> ms.languageDescription
        ),
      modalityType,
      languageName,
      languageId,
      languageVarietyName,
      mimeType,
      characterEncoding,
      annotationType,
      annotationFormat,
      tagset,
      segmentationLevel,
      conformanceToStandardsBestPractices
    )
  )

  msxml.outputInfo --> (
    ms.outputInfo > node(frag("outputInfo")) (
      rdf_type > ms.OutputInfo,
      mediaType,
      objectMap(msxml.resourceType, ms.resourceType,
        "corpus" -> ms.corpus,
        "lexicalConceptualResource" -> ms.lexicalConceptualResource,
        "languageDescription" -> ms.languageDescription
        ),
      modalityType,
      languageName,
      languageId,
      languageVarietyName,
      mimeType,
      characterEncoding,
      annotationType,
      annotationFormat,
      tagset,
      segmentationLevel,
      conformanceToStandardsBestPractices
    )
  )

  msxml.toolServiceEvaluationInfo --> (
    boolMap(msxml.evaluated, ms.evaluated),
    objectMap(msxml.evaluationLevel, ms.evaluationLevel,
      "technological" -> ms.technological,
      "usage" -> ms.usage,
      "impact" -> ms.impact,
      "diagnostic" -> ms.diagnostic
      ),
    objectMap(msxml.evaluationType, ms.evaluationType,
      "glassBox" -> ms.glassBox,
      "blackBox" -> ms.blackBox
      ),
    objectMap(msxml.evaluationCriteria, ms.evaluationCriteria,
      "extrinsic" -> ms.extrinsic,
      "intrinsic" -> ms.intrinsic
      ),
    objectMap(msxml.evaluationMeasure, ms.evaluationMeasure,
      "human" -> ms.human,
      "automatic" -> ms.automatic
      ),
    handle(msxml.evaluationReport),
    handle(msxml.evaluationTool),
    stringMap(msxml.evaluationDetails, ms.evaluationDetails),
    handle(msxml.evaluator)
  )

  msxml.evaluationReport --> (
    ms.evaluationReport > node(frag("evaluationReport")) (
      documentationInfoType
    )
  )

  msxml.evaluationTool --> (
    ms.evaluationTool > targetResourceInfoType
  )

  msxml.evaluator --> (
    ms.evaluator > node(frag("evaluator")) (
      actorInfoType
    )
  )

  msxml.toolServiceOperationInfo --> (
    objectMap(msxml.operatingSystem, ms.operatingSystem,
      "os-independent" -> ms.`os-independent`,
      "windows" -> ms.windows,
      "linux" -> ms.linux,
      "unix" -> ms.unix,
      "mac-OS" -> ms.`mac-OS`,
      "other" -> ms.other
      ),
    handle(msxml.runningEnvironmentInfo),
    stringMap(msxml.runningTime, ms.runningTime)
  )

  msxml.toolServiceCreationInfo --> (
    stringMap(msxml.implementationLanguage, ms.implementationLanguage),
    stringMap(msxml.formalism, ms.formalism),
    handle(msxml.originalSource),
    stringMap(msxml.creationDetails, ms.creationDetails)
  ) 

  // Derived from resourceTypes/mediaTypes/AudioSpecific.xsd
  
  msxml.audioContentInfo --> (
    objectMap(msxml.speechItems, ms.speechItems,
      "isolatedWords" -> ms.isolatedWords,
      "isolatedDigits" -> ms.isolatedDigits,
      "naturalNumbers" -> ms.naturalNumbers,
      "properNouns" -> ms.properNouns,
      "applicationWords" -> ms.applicationWords,
      "phoneticallyRichSentences" -> ms.phoneticallyRichSentences,
      "phoneticallyRichWords" -> ms.phoneticallyRichWords,
      "phoneticallyBalancedSentences" -> ms.phoneticallyBalancedSentences,
      "moneyAmounts" -> ms.moneyAmounts,
      "creditCardNumbers" -> ms.creditCardNumbers,
      "telephoneNumbers" -> ms.telephoneNumbers,
      "yesNoQuestions" -> ms.yesNoQuestions,
      "vcvSequences" -> ms.vcvSequences,
      "freeSpeech" -> ms.freeSpeech,
      "other" -> ms.other
      ),
    objectMap(msxml.nonSpeechItems, ms.nonSpeechItems,
      "notes" -> ms.notes,
      "tempo" -> ms.tempo,
      "sounds" -> ms.sounds,
      "noise" -> ms.noise,
      "music" -> ms.music,
      "commercial " -> ms.commercial ,
      "other" -> ms.other
      ),
    stringMap(msxml.textualDescription, dc.description),
    objectMap(msxml.noiseLevel, ms.noiseLevel,
      "low" -> ms.low,
      "medium" -> ms.medium,
      "high" -> ms.high
    )
  )

  msxml.audioSizeInfo --> (
    ms.audioSizeInfo > node(frag("audioSizeInfo")) (
      rdf_type > ms.AudioSizeInfo,
      handle(msxml.sizeInfo),
      handle(msxml.durationOfEffectiveSpeechInfo),
      handle(msxml.durationOfAudioInfo)
    )
  )

  msxml.durationOfEffectiveSpeechInfo --> (
    // Not sure if this is actually 1:1, but shouldn't this be sizeInfoType anyway?
    //ms.durationOfEffectiveSpeechInfo > node(frag("durationOfEffectiveSpeechInfo")) (
    //  rdf_type > ms.DurationOfEffectiveSpeechInfo,
      intMap(msxml.size, ms.durationOfEffectiveSpeechInfo),
      objectMap(msxml.durationUnit, ms.durationOfEffectiveSpeechInfoUnit,
        "hours" -> ms.hours,
        "minutes" -> ms.minutes,
        "seconds" -> ms.seconds
      )
    //)
  )

  msxml.durationOfAudioInfo --> (
    // Not sure if this is actually 1:1, but shouldn't this be sizeInfoType anyway?
    //ms.durationOfAudioInfo > node(frag("durationOfAudioInfo")) (
    //  rdf_type > ms.DurationOfAudioInfo,
      intMap(msxml.size, ms.durationOfAudioInfo),
      objectMap(msxml.durationUnit, ms.durationOfAudioInfoUnit,
        "hours" -> ms.hours,
        "minutes" -> ms.minutes,
        "seconds" -> ms.seconds
      )
    //)
  )


  msxml.audioFormatInfo --> (
    ms.audioFormatInfo > node(frag("audioFormatInfo")) (
      rdf_type > ms.AudioFormatInfo,
      rdf_type > media.MediaResource,
      stringMap(msxml.mimeType, dc.format),
      objectMap(msxml.signalEncoding, ms.signalEncoding,
        "aLaw" -> ms.aLaw,
        "linearPCM" -> ms.linearPCM,
        "µ-law" -> ms.`mu-law`,
        "ADPCM" -> ms.ADPCM,
        "other" -> ms.other
        ),
      intMap(msxml.samplingRate, media.samplingRate),
      intMap(msxml.quantization, media.averageBitRate),
      objectMap(msxml.byteOrder, ms.byteOrder,
        "littleEndian" -> ms.littleEndian,
        "bigEndian" -> ms.bigEndian
        ),
      objectMap(msxml.signConvention, ms.signConvention,
        "signedInteger" -> ms.signedInteger,
        "unsignedInteger" -> ms.unsignedInteger,
        "floatingPoint" -> ms.floatingPoint
        ),
      handle(msxml.compressionInfo),
      objectMap(msxml.audioQualityMeasuresIncluded, ms.audioQualityMeasuresIncluded,
        "SNR" -> ms.SNR,
        "crossTalk" -> ms.crossTalk,
        "clippingRate" -> ms.clippingRate,
        "backgroundNoise" -> ms.backgroundNoise,
        "other" -> ms.other
        ),
      intMap(msxml.numberOfTracks, media.numberOfTracks),
      objectMap(msxml.recordingQuality, ms.recordingQuality,
        "veryLow" -> ms.veryLow,
        "low" -> ms.low,
        "medium" -> ms.medium,
        "high" -> ms.high,
        "veryHigh" -> ms.veryHigh
        ),
      handle(msxml.sizePerAudioFormat)
    )
  )

  msxml.sizePerAudioFormat --> (
    ms.sizePerAudioFormat > node(frag("sizePerAudioFormat")) (
      sizeInfoType
    )
  )

  msxml.audioClassificationInfo --> (
    ms.audioClassificationInfo > node(frag("audioClassificationInfo")) (
      rdf_type > ms.AudioClassificationInfo,
      dataMap(msxml.audioGenre, ms.audioGenre,
        "speech" -> "Speech",
        "humanNonSpeech" -> "Human Non-Speech",
        "noise" -> "Noise",
        "animalVocalizations" -> "Animal Vocalizations",
        "song" -> "Song",
        "instrumentalMusic" -> "Instrumental Music",
        "other" -> "Other"
        ),
      dataMap(msxml.speechGenre, ms.speechGenre,
        "broadcastNews" -> "Broadcast News",
        "meeting" -> "Meeting",
        "lecture" -> "Lecture",
        "emotionalExpressive" -> "Emotional Expressive",
        "airTrafficControl" -> "Air Traffic Control",
        "conversation" -> "Conversation",
        "roundtable" -> "Roundtable",
        "interview" -> "Interview",
        "debate" -> "Debate",
        "call-in" -> "Call-in",
        "questionAnswer" -> "Question Answer",
        "presentation" -> "Presentation",
        "narrative" -> "Narrative"
        ),
      stringMap(msxml.subject_topic, dc.subject),
      stringMap(msxml.register, ms.register),
      conformanceToClassificationScheme,
      handle(msxml.sizePerAudioClassification)
    )
  )

  msxml.sizePerAudioClassification --> (
    ms.sizePerAudioClassification > node(frag("sizePerAudioClassification"))(
      sizeInfoType
    )
  )

  // Derived from resourceTypes/mediaTypes/ImageSpecific.xsd

  msxml.imageContentInfo --> (
    stringMap(msxml.typeOfImageContent, ms.typeOfImageContent),
    objectMap(msxml.textIncludedInImage, ms.textIncludedInImage,
      "captions" -> ms.captions,
      "subtitles" -> ms.subtitles,
      "captureTime" -> ms.captureTime,
      "none" -> ms.none
      ),
    handle(msxml.staticElementInfo)
  )

  msxml.staticElementInfo --> (
    ms.staticElementInfo > node(frag("staticElementInfo")) (
      rdf_type > ms.StaticElementInfo,
      stringMap(msxml.typeOfElement, dc.`type`),
      objectMap(msxml.bodyParts, ms.bodyParts,
        "arms" -> ms.arms,
        "face" -> ms.face,
        "feet" -> ms.feet,
        "hands" -> ms.hands,
        "head" -> ms.head,
        "legs" -> ms.legs,
        "mouth" -> ms.mouth,
        "wholeBody" -> ms.wholeBody,
        "none" -> ms.none
        ),
      stringMap(msxml.faceViews, ms.faceViews),
      stringMap(msxml.faceExpressions, ms.faceExpressions),
      stringMap(msxml.artifactParts, ms.artifactParts),
      stringMap(msxml.landscapeParts, ms.landscapeParts),
      stringMap(msxml.personDescription, ms.personDescription),
      stringMap(msxml.thingDescription, ms.thingDescription),
      stringMap(msxml.organizationDescription, ms.organizationDescription),
      stringMap(msxml.eventDescription, ms.eventDescription)
    )
  )

  msxml.imageFormatInfo --> (
    ms.imageFormatInfo > node(frag("imageFormatInfo")) (
      rdf_type > ms.ImageFormatInfo,
      stringMap(msxml.mimeType, dc.format),
      objectMap(msxml.colourSpace, ms.colourSpace,
        "RGB" -> ms.RGB,
        "CMYK" -> ms.CMYK,
        "4:2:2" -> ms.`4:2:2`,
        "YUV" -> ms.YUV
        ),	
      stringMap(msxml.colourDepth, ms.colourDepth),
      handle(msxml.compressionInfo),
      handle(msxml.resolutionInfo),
      objectMap(msxml.visualModelling, ms.visualModelling,
        "2D" -> ms.`2D`,
        "3D" -> ms.`3D`
        ),
      objectMap(msxml.rasterOrVectorGraphics, ms.rasterOrVectorGraphics,
        "raster" -> ms.raster,
        "vector" -> ms.vector
        ),
      objectMap(msxml.quality, ms.quality,
        "veryLow" -> ms.veryLow,
        "low" -> ms.low,
        "medium" -> ms.medium,
        "high" -> ms.high,
        "veryHigh" -> ms.veryHigh
        ),
      handle(msxml.sizePerImageFormat)
    )
  )

  msxml.sizePerImageFormat --> (
    ms.sizePerImageFormat > node(frag("sizePerImageFormat")) (
      sizeInfoType
    )
  )

  msxml.imageClassificationInfo --> (
    ms.imageClassificationInfo > node(frag("imageClassificationInfo")) (
      rdf_type > ms.ImageClassificationInfo,
      stringMap(msxml.imageGenre, ms.imageGenre),
      stringMap(msxml.subject_topic, dc.subject),
      conformanceToClassificationScheme,
      handle(msxml.sizePerImageClassification)
    )
  )

  msxml.sizePerImageClassification --> (
    ms.sizePerImageClassification > node(frag("sizePerImageClassification")) (
      sizeInfoType
    )
  )

  // Derived from resourceTypes/mediaTypes/TextNgramSpecific.xsd

  msxml.ngramInfo --> (
    ms.ngramInfo > node(frag("ngramInfo")) (
      rdf_type > ms.NgramInfo,
      objectMap(msxml.baseItem, ms.baseItem,
        "word" -> ms.word,
        "syllable" -> ms.syllable,
        "letter" -> ms.letter,
        "phoneme" -> ms.phoneme,
        "other" -> ms.other
        ),
      intMap(msxml.order, ms.order),
      doubleMap(msxml.perplexity, ms.perplexity),
      boolMap(msxml.isFactored, ms.isFactored),
      stringMap(msxml.factors, ms.factors),
      stringMap(msxml.smoothing, ms.smoothing),
      boolMap(msxml.interpolated, ms.interpolated)
    )
  )

  // Derived from resourceTypes/mediaTypes/TextNumericalSpecific.xsd

  msxml.textNumericalContentInfo --> (
    stringMap(msxml.typeOfTextNumericalContent, ms.typeOfTextNumericalContent)
  )

  msxml.textNumericalFormatInfo --> (
    ms.textNumericalFormatInfo > node(frag("textNumericalFormatInfo")) (
      rdf_type > ms.TextNumericalFormatInfo,
      stringMap(msxml.mimeType, dc.format),
      handle(msxml.sizePerTextNumericalFormat)
    )
  )

  msxml.sizePerTextNumericalFormat --> (
    ms.sizePerTextNumericalFormat > node(frag("sizePerTextNumericalFormat")) (
      sizeInfoType
    )
  )

  // Derived from resourceTypes/mediaTypes/TextSpecific.xsd

  msxml.textFormatInfo --> (
    ms.textFormatInfo > node(frag("textFormatInfo")) (
      rdf_type > ms.TextFormatInfo,
      stringMap(msxml.mimeType, dc.format),
      handle(msxml.sizePerTextFormat)
    )
  )

  msxml.sizePerTextFormat --> (
    ms.sizePerTextFormat > node(frag("sizePerTextFormat")) (
      sizeInfoType
    )
  )

  msxml.textClassificationInfo --> (
    ms.textClassificationInfo > node(frag("textClassificationInfo")) (
      rdf_type > ms.TextClassificationInfo,
      stringMap(msxml.textGenre, ms.textGenre),
      stringMap(msxml.textType, dc.`type`),
      stringMap(msxml.register, ms.register),
      stringMap(msxml.subject_topic, dc.subject),
      conformanceToClassificationScheme,
      handle(msxml.sizePerTextClassification)
    )
  )

  msxml.sizePerTextClassification --> (
    ms.sizePerTextClassification > node(frag("sizePerTextClassification")) (
      sizeInfoType
    )
  )

  // Derived from resourceTypes/mediaTypes/VideoSpecific.xsd

  msxml.videoContentInfo --> (
    stringMap(msxml.typeOfVideoContent, dc.`type`),
    objectMap(msxml.textIncludedInVideo, ms.textIncludedInVideo,
      "captions" -> ms.captions,
      "subtitles" -> ms.subtitles,
      "none" -> ms.none
      ),
    handle(msxml.dynamicElementInfo)
  )

  msxml.dynamicElementInfo --> (
    ms.dynamicElementInfo > node(frag("dynamicElementInfo")) (
      rdf_type > ms.DynamicElementInfo,
      stringMap(msxml.typeOfElement, dc.`type`),
      objectMap(msxml.bodyParts, ms.bodyParts,
        "arms" -> ms.arms,
        "face" -> ms.face,
        "feet" -> ms.feet,
        "hands" -> ms.hands,
        "head" -> ms.head,
        "legs" -> ms.legs,
        "mouth" -> ms.mouth,
        "wholeBody" -> ms.wholeBody,
        "none" -> ms.none
        ),
      stringMap(msxml.distractors, ms.distractors),
      stringMap(msxml.interactiveMedia, ms.interactiveMedia),
      stringMap(msxml.faceViews, ms.faceViews),
      stringMap(msxml.faceExpressions, ms.faceExpressions),
      stringMap(msxml.bodyMovement, ms.bodyMovement),
      stringMap(msxml.gestures, ms.gestures),
      stringMap(msxml.handArmMovement, ms.handArmMovement),
      stringMap(msxml.handManipulation, ms.handManipulation),
      stringMap(msxml.headMovement, ms.headMovement),
      stringMap(msxml.eyeMovement, ms.eyeMovement),
      intMap(msxml.posesPerSubject, ms.posesPerSubject)
    )
  )

  msxml.videoFormatInfo --> (
    ms.videoFormatInfo > node(frag("videoFormatInfo")) (
      rdf_type > ms.VideoFormatInfo,
      stringMap(msxml.mimeType, dc.format),
      objectMap(msxml.colourSpace, ms.colourSpace,
        "RGB" -> ms.RGB,
        "CMYK" -> ms.CMYK,
        "4:2:2" -> ms.`4:2:2`,
        "YUV" -> ms.YUV
        ),	
      intMap(msxml.colourDepth, ms.colourDepth),
      intMap(msxml.frameRate, ms.frameRate),
      handle(msxml.resolutionInfo),
      objectMap(msxml.visualModelling, ms.visualModelling,
        "2D" -> ms.`2D`,
        "3D" -> ms.`3D`
        ),
      boolMap(msxml.fidelity, ms.fidelity),
      handle(msxml.compressionInfo),
      handle(msxml.sizePerVideoFormat)
    )
  )
  
  msxml.sizePerVideoFormat --> (
    ms.sizePerVideoFormat > node(frag("sizePerVideoFormat")) (
      sizeInfoType
    )
  )

  msxml.videoClassificationInfo --> (
    ms.videoClassificationInfo > node(frag("videoClassificationInfo")) (
      rdf_type > ms.VideoClassificationInfo,
      stringMap(msxml.videoGenre, ms.videoGenre),
      stringMap(msxml.subject_topic, dc.subject),
      conformanceToClassificationScheme,
      handle(msxml.sizePerVideoClassification)
    )
  )

  msxml.sizePerVideoClassification --> (
    ms.sizePerVideoClassification > node(frag("sizePerVideoClassification")) (
      sizeInfoType
    )
  )

  // Derived from resourceTypes/mediaTypes/Corpus/corpusAudioInfo.xsd

  msxml.corpusAudioInfo --> (
    ms.corpusAudioInfo > node(frag("corpusAudioInfo")) (
      rdf_type > ms.CorpusAudioInfo,
      ms.mediaType > ms.audio,
      xmlAtt("mediaType","audio"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.audioSizeInfo),
      handle(msxml.audioContentInfo),
      handle(msxml.settingInfo),
      handle(msxml.audioFormatInfo),
      handle(msxml.annotationInfo),
      handle(msxml.domainInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.audioClassificationInfo),
      handle(msxml.recordingInfo),
      handle(msxml.captureInfo),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/Corpus/corpusImageInfo.xsd

  msxml.corpusImageInfo --> (
    ms.corpusImageInfo > node(frag("corpusImageInfo")) (
      rdf_type > ms.CorpusImageInfo,
      ms.mediaType > ms.image,
      xmlAtt("mediaType","image"),
      handle(msxml.modalityInfo),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.sizeInfo),
      handle(msxml.imageContentInfo),
      handle(msxml.imageFormatInfo),
      handle(msxml.annotationInfo),
      handle(msxml.domainInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.imageClassificationInfo),
      handle(msxml.captureInfo),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo)
    )
  )
  
  // Derived from resourceTypes/mediaTypes/Corpus/corpusTextInfo.xsd
      
  msxml.corpusTextInfo --> (
    ms.corpusTextInfo > node(frag("corpusTextInfo")) (
      rdf_type > ms.CorpusTextInfo,
      ms.mediaType > ms.text,
      xmlAtt("mediaType","text"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.textFormatInfo),
      handle(msxml.characterEncodingInfo),
      handle(msxml.annotationInfo),
      handle(msxml.domainInfo),
      handle(msxml.textClassificationInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/Corpus/corpusTextNgramInfo.xsd
      
  msxml.corpusTextNgramInfo --> (
    ms.corpusTextNgramInfo > node(frag("corpusTextNgramInfo")) (
      rdf_type > ms.CorpusTextNgramInfo,
      ms.mediaType > ms.textNgram,
      xmlAtt("mediaType","textNgram"),
      handle(msxml.ngramInfo),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.textFormatInfo),
      handle(msxml.characterEncodingInfo),
      handle(msxml.annotationInfo),
      handle(msxml.domainInfo),
      handle(msxml.textClassificationInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.creationInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/Corpus/corpusTextNumericalInfo.xsd
  
  msxml.corpusTextNumericalInfo --> (
    ms.corpusTextNumericalInfo > node(frag("corpusTextNumericalInfo")) (
      rdf_type > ms.CorpusTextNumericalInfo,
      ms.mediaType > ms.textNumerical,
      xmlAtt("mediaType","textNumerical"),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.textNumericalContentInfo),
      handle(msxml.textNumericalFormatInfo),
      handle(msxml.recordingInfo),
      handle(msxml.captureInfo),
      handle(msxml.creationInfo),
      handle(msxml.annotationInfo),
      handle(msxml.linkToOtherMediaInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/Corpus/corpusTextVideoInfo.xsd
  
  msxml.corpusVideoInfo --> (
    ms.corpusVideoInfo > node(frag("corpusVideoInfo")) (
      rdf_type > ms.CorpusVideoInfo,
      ms.mediaType > ms.video,
      xmlAtt("mediaType","video"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.videoContentInfo),
      handle(msxml.settingInfo),
      handle(msxml.videoFormatInfo),
      handle(msxml.annotationInfo),
      handle(msxml.domainInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.videoClassificationInfo),
      handle(msxml.recordingInfo),
      handle(msxml.captureInfo),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/languageDescription/LanguageDescTextInfo.xsd

  msxml.languageDescriptionTextInfo --> (
    ms.languageDescriptionTextInfo > node(frag("languageDescriptionTextInfo")) (
      rdf_type > ms.LanguageDescriptionTextInfo,
      ms.mediaType > ms.text,
      xmlAtt("mediaType","text"),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.textFormatInfo),
      handle(msxml.characterEncodingInfo),
      handle(msxml.domainInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/languageDescription/LanguageDescimageInfo.xsd

  msxml.languageDescriptionImageInfo --> (
    ms.languageDescriptionImageInfo > node(frag("languageDescriptionImageInfo")) (
      rdf_type > ms.LanguageDescriptionImageInfo,
      ms.mediaType > ms.image,
      xmlAtt("mediaType","image"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.imageContentInfo),
      handle(msxml.imageFormatInfo),
      handle(msxml.domainInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.timeCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/languageDescription/LanguageDescvideoInfo.xsd

  msxml.languageDescriptionVideoInfo --> (
    ms.languageDescriptionVideoInfo > node(frag("languageDescriptionVideoInfo")) (
      rdf_type > ms.LanguageDescriptionVideoInfo,
      ms.mediaType > ms.video,
      xmlAtt("mediaType","video"),
      handle(msxml.creationInfo),
      handle(msxml.linkToOtherMediaInfo),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.videoContentInfo),
      handle(msxml.videoFormatInfo),
      handle(msxml.domainInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.timeCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/lexicalConceptualResource/lexicalConceptualResourceAudioInfo.xsd

  msxml.lexicalConceptualResourceAudioInfo --> (
    ms.lexicalConceptualResourceAudioInfo > node(frag("lexicalConceptualResourceAudioInfo")) (
      rdf_type > ms.LexicalConceptualResourceAudioInfo,
      ms.mediaType > ms.audio,
      xmlAtt("mediaType","audio"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.audioContentInfo),
      handle(msxml.audioFormatInfo),
      handle(msxml.domainInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.timeCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/lexicalConceptualResource/lexicalConceptualResourceImageInfo.xsd

  msxml.lexicalConceptualResourceImageInfo --> (
    ms.lexicalConceptualResourceImageInfo > node(frag("lexicalConceptualResourceImageInfo")) (
      rdf_type > ms.LexicalConceptualResourceImageInfo,
      ms.mediaType > ms.image,
      xmlAtt("mediaType","image"),
      handle(msxml.modalityInfo),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.sizeInfo),
      handle(msxml.imageContentInfo),
      handle(msxml.imageFormatInfo),
      handle(msxml.domainInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.timeCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/lexicalConceptualResource/lexicalConceptualResourceTextInfo.xsd
  
  msxml.lexicalConceptualResourceTextInfo --> (
    ms.lexicalConceptualResourceTextInfo > node(frag("lexicalConceptualResourceTextInfo")) (
      rdf_type > ms.LexicalConceptualResourceTextInfo,
      ms.mediaType > ms.text,
      xmlAtt("mediaType","text"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.textFormatInfo),
      handle(msxml.characterEncodingInfo),
      handle(msxml.domainInfo),
      handle(msxml.timeCoverageInfo),
      handle(msxml.geographicCoverageInfo)
    )
  )

  // Derived from resourceTypes/mediaTypes/lexicalConceptualResource/lexicalConceptualResourceVideoInfo.xsd

  msxml.lexicalConceptualResourceVideoInfo --> (
    ms.lexicalConceptualResourceVideoInfo > node(frag("lexicalConceptualResourceVideoInfo")) (
      rdf_type > ms.LexicalConceptualResourceVideoInfo,
      ms.mediaType > ms.video,
      xmlAtt("mediaType","video"),
      handle(msxml.lingualityInfo),
      handle(msxml.languageInfo),
      handle(msxml.modalityInfo),
      handle(msxml.sizeInfo),
      handle(msxml.videoContentInfo),
      handle(msxml.videoFormatInfo),
      handle(msxml.domainInfo),
      handle(msxml.geographicCoverageInfo),
      handle(msxml.timeCoverageInfo)
    )
  )
}
