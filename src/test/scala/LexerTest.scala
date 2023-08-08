import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  test("handles empty input") {
    val tokens = Lexer("").lex()
    assert(tokens.head.kind == TokenKind.Eof)
  }

  test("handles whitespace") {
    val tokens = Lexer("   ").lex()
    val target = List(Token(TokenKind.Eof, "", 2))
    assert(tokens == target)
  }

  test("handles single character tokens") {
    val tokens = Lexer("+ - / *").lex()
    val target = List(
      Token(TokenKind.Plus, "+", 0),
      Token(TokenKind.Minus, "-", 2),
      Token(TokenKind.Slash, "/", 4),
      Token(TokenKind.Star, "*", 6),
      Token(TokenKind.Eof, "", 6),
    )
    assert(tokens == target)
  }

  test("handles names") {
    val tokens = Lexer("flox").lex()
    val target = List(
      Token(TokenKind.Name, "flox", 0),
      Token(TokenKind.Eof, "", 3)
    )
    assert(tokens == target)
  }

  test("handles one letter names") {
    val tokens = Lexer("x").lex()
    val target = List(
      Token(TokenKind.Name, "x", 0),
      Token(TokenKind.Eof, "", 0)
    )
    assert(tokens == target)
  }

  test("handles integers") {
    val tokens = Lexer("123").lex()
    val target = List(
      Token(TokenKind.Int, "123", 0),
      Token(TokenKind.Eof, "", 2)
    )
    assert(tokens == target)
  }
  
  test("handles single digit integers") {
    val tokens = Lexer("1").lex()
    val target = List(
      Token(TokenKind.Int, "1", 0),
      Token(TokenKind.Eof, "", 0)
    )
    assert(tokens == target)
  }

  test("handles arrow") {
    val tokens = Lexer("=>").lex()
    val target = List(Token(
      TokenKind.Arrow, "=>", 0),
      Token(TokenKind.Eof, "", 1)
    )
    assert(tokens == target)
  }
  
  test("handles keywords") {
    val tokens = Lexer("let").lex()
    val target = List(Token(
      TokenKind.LetKeyword, "let", 0),
      Token(TokenKind.Eof, "", 2)
    )
    assert(tokens == target)
  }

  test("handles unknown symbols") {
    val tokens = Lexer("€").lex()
    val target = List(
      Token(TokenKind.Err, "€", 0),
      Token(TokenKind.Eof, "", 0)
    )
    assert(tokens == target)
  }
}
