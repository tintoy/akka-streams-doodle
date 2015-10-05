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
                  case TextTokens.Whitespace(WhitespaceText) => println(s"[Whitespace] '${WhitespaceText}'")
                  case TextTokens.Punctuation(punctuationText) => println(s"[Punctuation] '${punctuationText}'")
                  case TextTokens.End => println("[End]")
                }
              }
   */
  def tokenize(text: IndexedSeq[Char]): Seq[Token] = {
    /*
     * AF:Bug when dealing with single-quote characters. FIXME!
     */

    import scala.annotation.tailrec

    // Generate a lazily-evaluated stream of tokens.
    // I think this solution may have issues in terms of running out of stack space for large texts, but it's a start.
    val tokenStream: Stream[Token] = {
      def readToken(index: Int, previousToken: Token): Stream[Token] = {
        if (index < text.length) {
          val currentChar = text(index)
          var yieldToken = false

          val currentToken = previousToken match {
            case TextTokens.Start =>
              currentChar match {
                case AlphanumericCharacter(value) => TextTokens.Word(value.toString)
                case WhitespaceCharacter(value) => TextTokens.Whitespace(value.toString)
                case value: Char => TextTokens.Punctuation(value.toString)
              }
            case TextTokens.Whitespace(currentWhitespaceText) =>
              currentChar match {
                case AlphanumericCharacter(value) =>
                  yieldToken = true

                  TextTokens.Word(value.toString)
                case WhitespaceCharacter(value) => TextTokens.Whitespace(currentWhitespaceText + value)
                case value: Char =>
                  yieldToken = true

                  TextTokens.Punctuation(value.toString)
              }
            case TextTokens.Word(currentWordText) =>
              currentChar match {
                case AlphanumericCharacter(value) => TextTokens.Word(currentWordText + value)
                case WhitespaceCharacter(value) =>
                  yieldToken = true

                  TextTokens.Whitespace(value.toString)
                case value: Char =>
                  yieldToken = true

                  TextTokens.Punctuation(currentChar.toString)
              }
            case Punctuation(currentPunctuationText) =>
              currentChar match {
                case AlphanumericCharacter(value) =>
                  yieldToken = true
                  TextTokens.Punctuation(currentPunctuationText + value)
                case WhitespaceCharacter(value) =>
                  yieldToken = true

                  TextTokens.Whitespace(value.toString)
                case value: Char => TextTokens.Punctuation(currentPunctuationText + value)
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

  /**
   * The base class for text segments
   */
  sealed abstract class Token(text: String)

  /**
   * Represents the start of the text
   */
  case object Start extends Token("")

  /**
   * Represents a word
   */
  case class Word(text:String) extends Token(text)

  /**
   * Represents punctuation
   */
  case class Punctuation(text: String) extends Token(text)

  /**
   * Represents 1 or more contiguous Whitespace characters
   */
  case class Whitespace(text: String) extends Token(text)

  /**
   * Singleton token representing the end of the text
   */
  case object End extends Token("")

  /**
   * Represents a character in the input stream
   * @param value The character value.
   */
  private sealed abstract class Character(value: Char)

  /**
   * Represents an alphanumeric character (A-Z,a-z,0-9).
   * @param value The character value.
   */
  private case class AlphanumericCharacter(value: Char) extends Character(value)

  /**
   * Extractor for AlphanumericCharacter.
   */
  private object AlphanumericCharacter {
    /**
     * Attempt to extract an Alphanumeric from the specified character value (if it is an alphanumeric character).
     * @param value The character value.
     * @return Some Alphanumeric, or None.
     */
    def unapply(value: Char): Option[AlphanumericCharacter] = {
      if (value.isLetterOrDigit) Some(AlphanumericCharacter(value))
      else None
    }
  }

  /**
   * Represents a Whitespace character (A-Z,a-z,0-9).
   * @param value The character value.
   */
  private case class WhitespaceCharacter(value: Char) extends Character(value)

  /**
   * Extractor for WhitespaceCharacter.
   */
  private object WhitespaceCharacter {
    /**
     * Attempt to extract an Alphanumeric from the specified character value (if it is an alphanumeric character).
     * @param value The character value.
     * @return Some Alphanumeric, or None.
     */
    def unapply(value: Char): Option[WhitespaceCharacter] = {
      if (value.isWhitespace) Some(WhitespaceCharacter(value))
      else None
    }
  }
}
