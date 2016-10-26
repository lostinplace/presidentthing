package wikiparser


object WikiParser {
  import fastparse.all._
  import WikiAST._
  import RegexParser._
  import WikiTokens._
  import StringNotIn._

  val normalExpression: P[Expression] = P(
    break | XML | formatExpression | templateInvocation | parenthetical | link | sentenceFragment
  )


  val newline = P( "\n" | "\r\n" | "\r" | "\f")
  val whitespace = P( " " | "\t").rep(min=1)
  val wsBlock = (whitespace|newline).rep(min=1)

  val break = P("<br" ~/ whitespace.? ~ (">" | "/>")) .map(_ => Break())

  val headingBlock = "=".rep(min=2).!.map { _.length}


  val chunk = normalExpression.rep(min=1, sep=whitespace.?) .map { Chunk(_:_*) }

  val urlChars = CharIn("""ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()%*+,;=""")

  val url = "http" ~ "s".? ~ "://" ~/ urlChars.!.rep.map(x=>{
    Chunk(Word(x.mkString))
  })

  val valueChunk = (url|chunk).rep(sep="\n")


  val expressionEnd = (TEMPLATE_END|LINK_END|ARG_SEPARATOR|newline|End)

//  val tokens = (WikiTokens.values.map({_.toString}).reduceLeft((memo,item)=>P(memo | P(item)) ) )

//  val word = (AnyChar.! ~ !tokens).rep.map {_.toString}

  val word = SNI(WikiTokens.values.map({_.toString}).toList:_*).map(x=>Word(x.toString))

  val sentenceFragment = word.rep(min=1, sep=whitespace).map { SentenceFragment(_:_*) }

  val parenthetical = P("(" ~/ whitespace.? ~ chunk ~/ whitespace.? ~ ")").map { Parenthetical(_) }

  val templateInvocation:P[TemplateInvocation] = P(TEMPLATE_START ~/ args ~/ wsBlock.?  ~/ TEMPLATE_END).map {
    TemplateInvocation(_:_*)
  }

  val indentBlock = newline ~ ":".rep(min=1) ~ whitespace.? ~ chunk

  val link = P(LINK_START ~/ linkArgs ~/ whitespace.? ~ LINK_END ).map { Link(_:_*) }

  val plainContent = (sentenceFragment | parenthetical | templateInvocation | link).rep(min=1, sep=whitespace.rep.?).map { Chunk(_:_*) }

  val italicContent = (ITALIC_QUOTES ~ whitespace.? ~ plainContent ~ whitespace.? ~ ITALIC_QUOTES) map { Italic(_) }

  val boldContent = (BOLD_QUOTES ~ whitespace.? ~ (plainContent | italicContent) ~ whitespace.? ~/ BOLD_QUOTES) map { Bold(_) }

  val formatExpression = boldContent | italicContent

  val listBullet = (newline ~ ("*"|":"|"#").rep(min=1) ~ whitespace ~/ chunk) .map { ListBullet(_) }

  val list = listBullet.rep(min=1).map { WikiList(_:_*) }

  val heading = ((newline |Start) ~ headingBlock ~ chunk ~ headingBlock)  .map {
    case (level, content, _) => Heading(content, level)
  }

  val key = SNI("=","|"," ","}}").map {Word(_)}

  val kvPair = key ~ whitespace.? ~ "=" ~ wsBlock.? ~ valueChunk.? map {
    case (word, Some(results)) => KVPair(word, Some(results))
    case (word, None) => KVPair(word, None)
  }

  val breakOutContent = (heading | list | indentBlock) ~ whitespace.? ~ &(expressionEnd)



  val lineSeparator = newline.rep(min=1) ~ !breakOutContent

  val normalOrBreakout = (normalExpression | breakOutContent).rep(sep=whitespace.?).map {Chunk(_:_*)}

  val arg = P(kvPair | normalOrBreakout)
  val argSeparator = wsBlock.? ~ ARG_SEPARATOR ~ wsBlock.?

  val linkArgs = P(chunk | "".!.map(x => { Word("") })).rep(min=1, sep=argSeparator ~ Pass)
  val args =  arg.rep(min=1, sep=argSeparator ~ Pass )

  val clearNewLine = P((newline ~ !(heading | list | indentBlock)).rep | newline)


  val XML = (Xml.Xml.Element | Xml.Xml.EntityRef | Xml.Xml.Comment ) .map(WikiXML(_))

  val chunkLine = chunk ~ whitespace.? ~ newline

  val newLineBreak = (whitespace.? ~ newline ~ whitespace.?).map(x=>Break())

  val wikiPage = (Start ~ (chunk | breakOutContent| newLineBreak).rep(min=1) ~ whitespace.? ~ End.opaque("end of file")) .map { WikiPage( _:_*) }

}
