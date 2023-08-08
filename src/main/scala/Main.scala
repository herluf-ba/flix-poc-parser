object Main {
  def main(args: Array[String]): Unit = {
    args match {
        case Array(x: String) => println("" + Lexer(x).lex())
        case Array() => throw new java.lang.IllegalArgumentException("Too few arguments!")
        case _ => throw new java.lang.IllegalArgumentException("Too many arguments!")
    }    
  }
}
