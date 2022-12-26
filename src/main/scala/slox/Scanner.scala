package com.craftinginterpreters.lox
import Single.*
import One.*
import Two.*
import Literal.*
import Keyword.*

object Keywords:
  val keywords = Map[String, Keyword](
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )

case class Scanner(val src: String):
  import Keywords.*
  def scanTokens(
      state: ScannerState = ScannerState("", src, 0),
      tokens: Tokens = Tokens(Seq())
  ): Tokens =
    val source = state.source
    if (source.isEmpty()) then return tokens
    val (tokenOption: Option[Token], _state: ScannerState) = scanToken(
      source.head,
      state
        .uLex(source.head.toString())
        .uSrc(source.tail)
        .uLin(state.line)
    )
    tokenOption match
      case Some(token) => scanTokens(_state, tokens :+ token)
      case _           => scanTokens(_state, tokens)

  private def scanToken(
      c: Char,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    c match
      case '(' => addToken(LEFT_PAREN, state)
      case ')' => addToken(RIGHT_PAREN, state)
      case '{' => addToken(LEFT_BRACE, state)
      case '}' => addToken(RIGHT_BRACE, state)
      case ',' => addToken(COMMA, state)
      case '.' => addToken(DOT, state)
      case '-' => addToken(MINUS, state)
      case '+' => addToken(PLUS, state)
      case ';' => addToken(SEMICOLON, state)
      case '*' => addToken(STAR, state)
      case '!' => addToken(BANG, BANG_EQUAL, state)
      case '=' => addToken(EQUAL, EQUAL_EQUAL, state)
      case '<' => addToken(LESS, LESS_EQUAL, state)
      case '>' => addToken(GREATER, GREATER_EQUAL, state)
      case '/' =>
        state.source.headOption match
          case Some('/') =>
            val (_, tail) = state.source.span(_ != '\n')
            (None, state.uSrc(state.source.tail))
          case None => addToken(SLASH, state.uSrc(""))
          case _    => addToken(SLASH, state)
      case ' ' | '\r' | '\t' => (None, state)
      case '\n' =>
        (None, state.uLin(state.line + 1))
      case '"'             => stringLiteral(c, state)
      case n if n.isDigit  => numberLiteral(n, state)
      case a if a.isLetter => scanIdentifier(a, state)
      case _ =>
        Slox().error(state.line, "Unexpected character.")
        (None, state)

  private def addToken(
      _type: TokenType,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    (Some(Token(_type, state.lexeme, None, state.line)), state)

  private def addToken(
      _type: TokenType,
      lexeme: String,
      literal: Any,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    (Some(Token(_type, lexeme, literal, state.line)), state)

  private def addToken(
      op1: One,
      op2: Two,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    val source = state.source
    val line = state.line
    source.head match
      case '=' =>
        (
          Some(
            Token(op2, state.lexeme + source.head, None, line)
          ),
          state.uSrc(source.tail)
        )
      case _ => (Some(Token(op1, state.lexeme, None, line)), state)

  private def stringLiteral(c: Char, state: ScannerState) =
    val (prefix, suffix) = state.source.span(_ != '"')
    prefix match
      case _ if suffix.isEmpty =>
        Slox().error(state.line, "Unterminated string.")
        (None, state)
      case _ =>
        addToken(
          STRING,
          prefix,
          prefix,
          state
            .uSrc(suffix.tail)
            .uLin(state.line + prefix.count(_ == '\n'))
        )

  private def numberLiteral(digit: Char, state: ScannerState) =
    val number = digit + scanNumber(state.source)
    addToken(
      NUMBER,
      number,
      number.toDouble,
      state.uSrc(state.source.drop(number.length))
    )

  private def scanNumber(s: String) =
    val (integer, suffix) = s.span(c => c != '.' && c.isDigit)
    suffix match
      case _ if !(suffix.isEmpty) =>
        integer + (if (
                     suffix.head == '.' && suffix
                       .applyOrElse(1, _ => ' ')
                       .isDigit
                   )
                     "." + suffix.tail.takeWhile(_.isDigit)
                   else "")
      case _ => integer

  private def scanIdentifier(c: Char, state: ScannerState) =
    val (prefix, source) = state.source.span(_.isLetter)
    val identifier = c + prefix
    val _type = keywords.getOrElse(identifier, IDENTIFIER)
    addToken(_type, identifier, None, state.uSrc(source))

case class ScannerState(
    val lexeme: String,
    val source: String,
    val line: Int
):
  def uLex(lexeme: String) = this.copy(lexeme = lexeme)
  def uSrc(source: String) = this.copy(source = source)
  def uLin(line: Int) = this.copy(line = line)
