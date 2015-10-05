import scala.collection.Seq

/**
 * A basic parser for text.
 */
object TextTokens {
  /**
   * Tokenise the specified text into a sequence of TextTokens.
   * @param text An indexed sequence of characters representing the text to parse.
   * @return A sequence of TextTokens, ending with the TextParser.End token.
   * @example
   *          for (token <- TextTokens.tokenize(text)) {
                token match {
                  case TextTokens.Start => println("[Start]")
                  case TextTokens.Word(wordText) => println(s"[Word] '${wordText}'")
                  case TextTokens.Whitespace(whitespaceText) => println(s"[Whitespace] '${whitespaceText}'")
                  case TextTokens.Punctuation(punctuationText) => println(s"[Punctuation] '${punctuationText}'")
                  case TextTokens.End => println("[End]")
                }
              }
   */
  def tokenize(text: IndexedSeq[Char]): Seq[Token] = {
    /*
     * AF:Bug when dealing with single-quote characters. FIXME!
     */

    // Generate a lazily evaulated stream of tokens.
    // I think this solution may have issues in terms of running out of stack space for large texts, but it's a start.
    val tokenStream: Stream[Token] = {
      def readToken(index: Int, previousToken: Token): Stream[Token] = {
        if (index < text.length) {
          val currentChar = text(index)
          var yieldToken = false

          val currentToken = previousToken match {
            case TextTokens.Start =>
              if (currentChar.isWhitespace) {
                TextTokens.Whitespace(currentChar.toString)
              }
              else if (currentChar.isLetterOrDigit) {
                TextTokens.Word(currentChar.toString)
              }
              else {
                TextTokens.Punctuation(currentChar.toString)
              }
            case TextTokens.Whitespace(currentWhitespaceText) =>
              if (currentChar.isWhitespace) {
                TextTokens.Whitespace(currentWhitespaceText + currentChar)
              }
              else if (currentChar.isLetterOrDigit) {
                yieldToken = true
                TextTokens.Word(currentChar.toString)
              }
              else
              {
                yieldToken = true
                TextTokens.Punctuation(currentChar.toString)
              }
            case TextTokens.Word(currentWordText) =>
              if (currentChar.isWhitespace) {
                yieldToken = true
                TextTokens.Whitespace(currentChar.toString)
              }
              else if (currentChar.isLetterOrDigit) {
                TextTokens.Word(currentWordText + currentChar)
              }
              else
              {
                yieldToken = true
                TextTokens.Punctuation(currentChar.toString)
              }
            case Punctuation(currentPunctuationText) =>
              if (currentChar.isWhitespace) {
                yieldToken = true
                TextTokens.Whitespace(currentChar.toString)
              }
              else if (currentChar.isLetterOrDigit) {
                yieldToken = true
                TextTokens.Punctuation(currentPunctuationText + currentChar)
              }
              else
              {
                TextTokens.Punctuation(currentPunctuationText + currentChar)
              }
          }

          if (yieldToken)
            previousToken #:: readToken(index + 1, currentToken)
          else
            readToken(index + 1, currentToken)
        }
        else
          End #:: Stream.empty // Done
      }

      readToken(0, Start)
    }

    tokenStream
  }

  /** The base class for text segments */
  sealed abstract class Token(text: String)

  /** Represents the start of the text */
  case object Start extends Token("")

  /** Represents a word */
  case class Word(text:String) extends Token(text)

  /** Represents punctuation */
  case class Punctuation(text: String) extends Token(text)

  /** Represents 1 or more contiguous whitespace characters */
  case class Whitespace(text: String) extends Token(text)

  /** Singleton token representing the end of the text */
  case object End extends Token("")
}
