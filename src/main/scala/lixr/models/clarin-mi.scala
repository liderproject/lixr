package eu.liderproject.lixr

object ClarinMeertensSong extends Clarin {
  val ms = Namespace("http://purl.org/ms-lod/MetaShare.ttl#")

  cmd.Song --> (
    a > cmd.Song,
    stringMap(cmd.Summary, dc.description),
    stringMap(cmd.Musicnotation, cmd.Musicnotation),
    stringMap(cmd.Polyphony, cmd.Polyphony),
    stringMap(cmd.Audio, cmd.Audio),
    stringMap(cmd.Text, cmd.Text),
    handle(cmd.SongTitle),
    handle(cmd.Incipit),
    handle(cmd.Chorus),
    handle(cmd.Genres),
    handle(cmd.KeyWords),
    handle(cmd.Melody),
    handle(cmd.Stanza),
    handle(cmd.Authors),
    handle(cmd.LocationInSource),
    handle(cmd.Source),
    handle(cmd.Recording),
    handle(cmd.Language),
    handle(cmd.TechnicalMetadata)
  )

  cmd.SongTitle --> (
    stringMap(cmd.Original, dc.title),
    stringMap(cmd.Normalised, cmd.normalisedSongTitle)
  )

  cmd.Incipit --> (
    stringMap(cmd.Original, cmd.incipit),
    stringMap(cmd.Normalised, cmd.normalisedIncipit)
  )

  cmd.Chorus --> (
    stringMap(cmd.Original, cmd.chorus),
    stringMap(cmd.Normalised, cmd.normalisedChorus)
  )

  cmd.Genres --> (
    stringMap(cmd.Genre, cmd.genre)
  )

  cmd.KeyWords --> (
    stringMap(cmd.KeyWord, dcat.keyword)
  )

  cmd.Melody --> (
    node(frag("TuneIndication")) (
      a > cmd.TuneIndication,
      handle(cmd.TuneIndication)
    )
  )

  cmd.TuneIndication --> (
    node(frag("Tune")) (
      a > cmd.Tune,
      stringMap(cmd.Original, rdfs.label),
      stringMap(cmd.Normalised, cmd.normalisedTuneName),
      stringMap(cmd.StandardName, cmd.standardTuneName)
    )
  )

  cmd.Stanza --> (
    intMap(cmd.NumberOfStanzas, cmd.numberOfStanzas),
    handle(cmd.StanzaForm)
  )

  cmd.StanzaForm --> (
    cmd.stanzaForm > node(frag("StanzaForm")) (
      stringMap(cmd.RhymeScheme, cmd.rhymeScheme),
      stringMap(cmd.Accents, cmd.accents),
      stringMap(cmd.RhymeGender, cmd.rhymeGender),
      intMap(cmd.NumberOfVerses, cmd.numberOfVerses)
    )
  )

  cmd.Authors --> (
    handle(cmd.Author)
  )

  cmd.Author --> (
    dc.creator > node(frag("Author")) (
      a > foaf.Person,
      stringMap(cmd.Role, cmd.role),
      stringMap(cmd.Initials, cmd.initials),
      handle(cmd.Name)
    )
  )

  cmd.Name --> (
    stringMap(cmd.Original, foaf.name),
    stringMap(cmd.Normalised, cmd.normalisedName)
  )

  cmd.LocationInSource --> (
    stringMap(cmd.Pagenumber, cmd.pagenumber),
    stringMap(cmd.Songnumber, cmd.songnumber)
  )

  cmd.Source --> (
    dc.source > node(frag("Source")) (
      stringMap(cmd.CopyUsed, cmd.copyUsed),
      typedMap(cmd.Year, cmd.year, xsd.gYear),
      stringMap(cmd.Century, cmd.century),
      handle(cmd.Title),
      handle(cmd.Authors)
    )
  )

  cmd.Title --> (
    stringMap(cmd.Original, dc.title),
    stringMap(cmd.Normalised, cmd.normalisedTitle)
  )

  cmd.Recording --> (
    cmd.recording > node(frag("Recording")) (
      stringMap(cmd.Singer, cmd.singer),
      stringMap(cmd.Recorder, cmd.recorder),
      dateMap(cmd.Date, dc.date),
      handle(cmd.Location)
    )
  )

  cmd.Location --> (
    stringMap(cmd.Place, cmd.place),
    stringMap(cmd.Kloekecode, cmd.kloekecode)
  )

  cmd.Language --> (
    ms.languageInfo > node(frag("Language")) (
      stringMap(cmd.LanguageName, ms.languageName),
      stringMap(cmd.`ISO639-3`, ms.languageId),
      stringMap(cmd.Commenet, rdfs.comment)
    )
  )

  cmd.TechnicalMetadata --> (
    stringMap(cmd.MimeType, dc.format),
    stringMap(cmd.Quality, ms.quality),
    handle(cmd.Size),
    handle(cmd.SpeechTechnicalMetadata)
  )

  cmd.Size --> (
    handle(cmd.TotalSize),
    handle(cmd.Width),
    handle(cmd.Height)
  )

  cmd.TotalSize --> (
    cmd.totalSize > node(frag("TotalSize")) (
      doubleMap(cmd.Number, ms.size),
      stringMap(cmd.SizeUnit, ms.sizeUnit) // Should be an object property :(
    )
  )

  cmd.Width --> (
    cmd.width > node(frag("Width")) (
      doubleMap(cmd.Number, ms.size),
      stringMap(cmd.SizeUnit, ms.sizeUnit)
    )
  )

  cmd.Height --> (
    cmd.height > node(frag("Height")) (
      doubleMap(cmd.Number, ms.size),
      stringMap(cmd.SizeUnit, ms.sizeUnit)
    )
  )

  cmd.SpeechTechnicalMetadata --> (
    doubleMap(cmd.SamplingFrequency, ms.sampleRate),
    doubleMap(cmd.NumberOfChannels, ms.numberOfTracks),
    objectMap(cmd.ByteOrder, ms.byteOrder,
      "big endian" -> ms.bigEndian,
      "little endian" -> ms.littleEndian
    ),
    forall(cmd.Compression)(
      when(content === "lossless")(
        ms.compression > true,
        ms.compressionLoss > false
      ).or(content === "lossy")(
        ms.compression > true,
        ms.compressionLoss > true
      ).or(content === "none")(
        ms.compression > false
      )
    ),
    doubleMap(cmd.BitResolution, cmd.bitResolution),
    objectMap(cmd.SpeechCoding, ms.signalEncoding,
      "pcm" -> ms.linearPCM,
      "alaw" -> ms.aLaw,
      "mlaw" -> ms.`mu-law`,
      "signed integer" -> ms.signedInteger,
      "unsigned integer" -> ms.unsignedInteger,
      "other" -> ms.other)
    // Ignore second mime type as it is duplicate
    //stringMap(ms.MimeType, dc.format)
  )
    
  
      

}
