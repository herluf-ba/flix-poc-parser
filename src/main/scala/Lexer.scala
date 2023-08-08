import scala.collection.mutable.ArrayBuffer

// NOTE: This lexer does not support UTF-8 codepoints
// Design choices:
// - Eof is placed on the last actual character in input, so no virtual position is created

case class Lexer(input: String) {
  private var currentPos = 0 // the read head into input
  private var text = "" // content of the current token
  private var start = 0 // cursor at the start of the current token

  // reads the current cursor and places it in the text buffer before advancing
  private def advance(): Char = {
    val c = input(currentPos)
    text += c
    currentPos += 1
    c
  }

  // Checks if the substring after cursor is a specific string
  private def matches(s: String): Boolean = {
    val matches = input.slice(currentPos, currentPos + s.length) == s
    if (matches) {
      currentPos += s.length
      text += s
    }
    matches
  }

  // eats the text buffer and returns it along with its start position in the input
  private def eat(): (Int, String) = {
    val r = (start, text)
    text = ""
    start = currentPos
    r
  }

  // moves the cursor past whitespace
  private def whitespace(): Unit = {
    while (true) {
      if (currentPos > input.length - 1) {
        return
      }

      val c = input(currentPos)
      if (!c.isWhitespace) {
        start = currentPos
        return
      }
      
      currentPos += 1
    }
  }

  // puts a name into the text buffer
  private def name(start: Char): TokenKind = {
    var c = start
    while (currentPos < input.length && !c.isWhitespace) {
      c = advance()
    }

    TokenKind.Name
  }

  // puts an integer into the text buffer
  private def int(start: Char): TokenKind = {
    var c = start
    while (currentPos < input.length && c.isDigit) {
      c = advance()
    }

    TokenKind.Int
  }

  // produces the next token
  private def next(): Token = {
    whitespace()

    if (currentPos >= input.length) {
      return Token(TokenKind.Eof, "", currentPos - 1)
    }

    val c = advance()
    val kind = c match {
      case '(' => TokenKind.LParen
      case ')' => TokenKind.RParen
      case '{' => TokenKind.LCurly
      case '}' => TokenKind.RCurly
      case ';' => TokenKind.Semi
      case ',' => TokenKind.Comma
      case ':' => TokenKind.Colon
      case '+' => TokenKind.Plus
      case '-' => TokenKind.Minus
      case '/' => TokenKind.Slash
      case '*' => TokenKind.Star
      case '=' =>
        if (matches(">")) {
          TokenKind.Arrow
        } else {
          TokenKind.Eq
        }

      case 'f' if matches("n") => TokenKind.FnKeyword
      case 'l' if matches("et") => TokenKind.LetKeyword
      case 'r' if matches("eturn") => TokenKind.ReturnKeyword
      case 't' if matches("rue") => TokenKind.TrueKeyword
      case 'f' if matches("alse") => TokenKind.FalseKeyword

      case t if t.isLetter => name(t)
      case t if t.isDigit => int(t)

      case _ => TokenKind.Err
    }
    val (s, t) = eat()
    val tok = Token(kind, t, s)
    tok
  }

  def lex(): List[Token] = {
    val tokens = ArrayBuffer[Token]()
    var t = next()
    tokens += t

    while (t.kind != TokenKind.Eof) {
      t = next()
      tokens += t
    }

    currentPos = 0
    tokens.toList
  }
}
