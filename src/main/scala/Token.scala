sealed trait TokenKind

object TokenKind {
  case object LParen extends TokenKind
  case object RParen extends TokenKind
  case object LCurly extends TokenKind
  case object RCurly extends TokenKind
  
  case object Eq extends TokenKind
  case object Semi extends TokenKind
  case object Comma extends TokenKind
  case object Colon extends TokenKind
  case object Arrow extends TokenKind
  case object Plus extends TokenKind
  case object Minus extends TokenKind
  case object Star extends TokenKind
  case object Slash extends TokenKind

  case object FnKeyword extends TokenKind
  case object LetKeyword extends TokenKind
  case object ReturnKeyword extends TokenKind
  case object TrueKeyword extends TokenKind
  case object FalseKeyword extends TokenKind
  case object Name extends TokenKind
  case object Int extends TokenKind

  case object Err extends TokenKind
  
  case object Eof extends TokenKind
}

case class Token(
  kind: TokenKind,
  text: String,
  start: Int
)
