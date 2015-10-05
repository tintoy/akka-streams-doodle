import scala.collection.Seq

/**
 * A basic parser for text.
 */
object TextParser {
  /**
   * Parse the specified text into a sequence of TextTokens.
   * @param text A sequence of characters representing the text to parse.
   * @return A sequence of TextTokens, ending with the TextParser.End token.
   */
  def parse(text: IndexedSeq[Char]): Seq[TextParser.TextToken] = {
    // TODO: Work out how to implement a generator method (like C#'s yield return).

    // Since it appears that Scala doesn't natively support coroutines like C#'s yield return,
    // we'll try a different approach.
    // Maybe something like "for (currentChar <- text) { /* conditionally yield something here */ }"

    Seq(TextParser.End)
  }

  /** The base class for text segments */
  sealed abstract class TextToken

  /** Represents a word */
  case class Word(text:String) extends TextToken

  /** Represents punctuation */
  case class Punctuation(text: String) extends TextToken

  /** Represents 1 or more contiguous whitespace characters */
  case class Whitespace(length: Int) extends TextToken

  /** Singleton token representing the end of the text */
  case object End extends TextToken
}
