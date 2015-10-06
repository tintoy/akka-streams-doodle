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

    /**
     * Generate a lazily-evaluated stream of tokens, reading the next character in the text and appending a token to the token stream if required.
     * @param textIndex The index, in the text, of the character to read.
     * @param tokenStream The existing stream of tokens.
     * @param previousToken The previous token emitted into the stream.
     * @return The token stream (with a newly-parsed token appended to it, if appropriate).
     */
    @tailrec def nextCharacter(textIndex: Int, tokenStream: Stream[Token], previousToken: Token): Stream[Token] = {
      if (textIndex < text.length) {
        val currentChar = text(textIndex)
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

                TextTokens.Punctuation(value.toString)
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

        if (yieldToken) {
          nextCharacter(
            textIndex = textIndex + 1,
            tokenStream = tokenStream :+ previousToken,
            previousToken = currentToken
          )
        }
        else {
          nextCharacter(
            textIndex = textIndex + 1,
            tokenStream = tokenStream,
            previousToken = currentToken
          )
        }
      }
      else
        tokenStream :+ End
    }

    // Start of stream
    nextCharacter(
      tokenStream = Stream.empty,
      textIndex = 0,
      previousToken = Start
    )
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
    def unapply(value: Char): Option[Char] = {
      if (value.isLetterOrDigit) Some(value)
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
    def unapply(value: Char): Option[Char] = {
      if (value.isWhitespace) Some(value)
      else None
    }
  }
}
