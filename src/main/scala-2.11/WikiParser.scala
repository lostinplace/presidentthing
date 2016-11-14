package wikiparser


object WikiParser {
  import fastparse.all._
  import WikiAST._
  import RegexParser._
  import WikiTokens._
  import StringNotIn._

  val escapedApostrophe = P("""''""").map(x=>Word("'"))

  val timeline = P("<timeline>"~SNI("</timeline>") ~ "</timeline").map(x=>WikiXML(""))

  val garbage = P( StringIn("|-") | timeline | Xml.Xml.openTag) .map(x=>Word(""))


  val normalExpression: P[Expression] = P(
    timeline | break | XML | formatExpression | scriptTemplate | templateInvocation |
      parenthetical | link | sentenceFragment | escapedApostrophe | garbage
  )

  val easyNormalExpression: P[Expression] = P(
    timeline | break | XML | formatExpression | scriptTemplate| templateInvocation |
      parenthetical | link | easySentenceFragment | escapedApostrophe | garbage
  )

  val stringWithEndParen = SNI(TEMPLATE_START , TEMPLATE_END , LINK_START , LINK_END , ARG_SEPARATOR ,
    PARENS_START , HEADING , SPACE , ITALIC_QUOTES , BOLD_QUOTES , REF_START , NOWIKI_START , XML_START ,
    NEWLINE)

  val stringWithNoTokens = SNI(WikiTokens.values.map({_.toString}).toList:_*)

  val scriptTemplate = (WikiTokens.SCRIPT_TEMPLATE_START ~/ SNI("|}") ~ WikiTokens.SCRIPT_TEMPLATE_END).map(x=>Word(""))

  val newline = P( "\n" | "\r\n" | "\r" | "\f")

  val whitespace = P( " " | "\t").rep(min=1)
  val newLineBreak = (whitespace.? ~ newline ~ whitespace.?).map(x=>Break())
  val wsBlock = (whitespace|newline).rep(min=1)

  val break = P("<br" ~/ whitespace.? ~ (">" | "/>")) .map(_ => Break())

  val headingBlock = "=".rep(min=2).!.map { _.length}

  val chunk = normalExpression.rep(min=1, sep=whitespace.?) .map { Chunk(_:_*) }

  val easyChunk = easyNormalExpression.rep(min=1, sep=whitespace.?) .map { Chunk(_:_*) }

  val urlChars = CharIn("""ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()%*+,;=""")

  val url = "http" ~ "s".? ~ "://" ~/ urlChars.!.rep.map(x=>{
    Chunk(Word(x.mkString))
  })




  val expressionEnd = (TEMPLATE_END|LINK_END|ARG_SEPARATOR|newline|End)

//  val tokens = (WikiTokens.values.map({_.toString}).reduceLeft((memo,item)=>P(memo | P(item)) ) )

//  val word = (AnyChar.! ~ !tokens).rep.map {_.toString}

  val rigidWord = stringWithNoTokens.map(x=>Word(x.toString))

  val easyWord = stringWithEndParen.map(x=>Word(x.toString))

//  val word = anyCharNotInKnownTokens

  val sentenceFragment = rigidWord.rep(min=1, sep=whitespace).map { SentenceFragment(_:_*) }

  val easySentenceFragment = easyWord.rep(min=1, sep=whitespace).map { SentenceFragment(_:_*) }

  val endParenOrEOL = P(")" | ("" ~ &("\n"))  )

  val garbagePipe = P("|").map(x=>Word("|"))

  val parentheticalChunk = (normalExpression | garbagePipe).rep(min=1, sep=whitespace.?) .map { Chunk(_:_*) }

  val parenthetical = P("(" ~/ whitespace.? ~ parentheticalChunk ~/ whitespace.? ~ endParenOrEOL ).map {x=> Parenthetical(x) }

  val templateInvocation:P[TemplateInvocation] = P(TEMPLATE_START ~ wsBlock.? ~/ args ~/ wsBlock.?  ~/ TEMPLATE_END).map {
    TemplateInvocation(_:_*)
  }

  val indentBlock = newline ~ ":".rep(min=1) ~ whitespace.? ~ chunk

  val link = P(LINK_START ~/ ( "File".! ~ ":" ~ wsBlock.?).?  ~/ linkArgs ~/ whitespace.? ~ LINK_END ).map {
    case (Some(linkType), args) => Link(linkType, args:_*)
    case (_ , args) => Link("reference", args:_*)
  }

  val xmlComment = ("<!--" ~ SNI("-->") ~ "-->")

  val XML = (Xml.Xml.Element | Xml.Xml.EntityRef | Xml.Xml.Comment | xmlComment | Xml.Xml.ETag ) .map(WikiXML(_))

  val simpleBold = P(BOLD_QUOTES ~ easyWord ~ BOLD_QUOTES).map(Bold(_))

  val plainContent = (sentenceFragment | parenthetical | templateInvocation | link | XML | simpleBold).rep(min=1, sep=whitespace.rep.?).map { Chunk(_:_*) }

  val italicContent = (ITALIC_QUOTES ~ whitespace.? ~ plainContent ~ whitespace.? ~ ITALIC_QUOTES) map { Italic(_) }

  val boldContent = (BOLD_QUOTES ~ whitespace.? ~ (plainContent | italicContent) ~ whitespace.? ~/ BOLD_QUOTES) map { Bold(_) }

  val formatExpression = boldContent | italicContent

  val listBullet = (newline ~ ("*"|":"|"#").rep(min=1) ~ whitespace ~/ easyChunk) .map { ListBullet(_) }

  val list = listBullet.rep(min=1).map { WikiList(_:_*) }

  val otherChars = StringIn(")").!.map {Word(_)}



  val heading = ((newline |Start) ~ headingBlock ~ whitespace.? ~ chunk ~/ whitespace.? ~ headingBlock ~ xmlComment.?)  .map {
    case (level, content, _, _) => Heading(content, level)
  }

  val key = SNI("=","|"," ","}}").map {Word(_)}



  val valueChunk = (url|chunk|newLineBreak|XML).rep

  val kvPair = key ~ whitespace.? ~ "=" ~ (wsBlock.? | "\n".?) ~ valueChunk.? map {
    case (word, Some(results)) => KVPair(word, Some(results))
    case (word, None) => KVPair(word, None)
  }

  val breakOutContent = (heading | list | indentBlock) ~ whitespace.? ~ &(expressionEnd)

  val lineSeparator = newline.rep(min=1) ~ !breakOutContent

  val normalOrBreakout = (normalExpression | breakOutContent | newLineBreak).rep(sep=whitespace.?).map {Chunk(_:_*)}

  val arg = P(kvPair | normalOrBreakout)
  val argSeparator = wsBlock.? ~ ARG_SEPARATOR ~ wsBlock.?

  val nothingAsWord = P("").!.map(x => { Word("") })

  val newlineSeparatedChunkGroup = P(chunk|nothingAsWord).rep(sep=newline).map(Chunk(_:_*))

  val linkArgs = P(newlineSeparatedChunkGroup).rep(min=1, sep=argSeparator ~ Pass)
  val args =  arg.rep(min=1, sep=argSeparator ~ Pass )

  val clearNewLine = P((newline ~ !(heading | list | indentBlock)).rep | newline)





  val chunkLine = chunk ~ whitespace.? ~ newline

  val wikiPage = (Start ~ (easyChunk | breakOutContent| newLineBreak).rep(min=1) ~ whitespace.? ~ End.opaque("end of file")) .map { WikiPage( _:_*) }

}
