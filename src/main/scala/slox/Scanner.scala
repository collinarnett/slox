package com.craftinginterpreters.slox
import Single.*
import One.*
import Two.*
import _Literal.*
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
    val (tokenOption, _state) = scanToken(
      source.head,
      state
        .uLex(source.head.toString())
        .uSrc(source.tail)
        .uLin(state.line)
    )
    tokenOption match
      case Some(token): Some[Token] => scanTokens(_state, tokens :+ token)
      case _                        => scanTokens(_state, tokens)

  private def scanToken(
      c: Char,
      state: ScannerState
  ) =
    given ScannerState = state
    c match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(BANG)
      case '=' => addToken(EQUAL, EQUAL_EQUAL)
      case '<' => addToken(LESS, LESS_EQUAL)
      case '>' => addToken(GREATER, GREATER_EQUAL)
      case '/' =>
        state.source.headOption match
          case Some('/') =>
            val (_, tail) = state.source.span(_ != '\n')
            (None, state.uSrc(state.source.tail))
          case None => addToken(SLASH)(using state.uSrc(""))
          case _    => addToken(SLASH)
      case ' ' | '\r' | '\t' => (None, state)
      case '\n' =>
        (None, state.uLin(state.line + 1))
      case '"'             => stringLiteral(c)
      case n if n.isDigit  => numberLiteral(n)
      case a if a.isLetter => scanIdentifier(a)
      case _ =>
        Slox().error(state.line, "Unexpected character.")
        (None, state)

  private def addToken(
      _type: TokenType
  )(using s: ScannerState) =
    (Token(_type, s.lexeme, None, s.line), s)

  private def addToken(
      _type: TokenType,
      lexeme: String,
      literal: Any
  )(using s: ScannerState) =
    (Token(_type, lexeme, literal, s.line), s)

  private def addToken(
      op1: One,
      op2: Two
  )(using s: ScannerState) =
    val source = s.source
    val line = s.line
    source.head match
      case '=' =>
        (
          Token(op2, s.lexeme + source.head, None, line),
          s.uSrc(source.tail)
        )
      case _ => (Token(op1, s.lexeme, None, line), s)

  private def stringLiteral(c: Char)(using s: ScannerState) =
    val (prefix, suffix) = s.source.span(_ != '"')
    prefix match
      case _ if suffix.isEmpty =>
        Slox().error(s.line, "Unterminated string.")
        (None, s)
      case _ =>
        addToken(
          STRING,
          prefix,
          prefix
        )(using
          s
            .uSrc(suffix.tail)
            .uLin(s.line + prefix.count(_ == '\n'))
        )

  private def numberLiteral(digit: Char)(using s: ScannerState) =
    val number = s"$digit${scanNumber(s.source)}"
    addToken(
      NUMBER,
      number,
      number.toDouble
    )(using s.uSrc(s.source.drop(number.length)))

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

  private def scanIdentifier(c: Char)(using s: ScannerState) =
    val (prefix, source) = s.source.span(_.isLetter)
    val identifier = s"$c$prefix"
    val _type = keywords.getOrElse(identifier, IDENTIFIER)
    addToken(_type, identifier, None)(using s.uSrc(source))

case class ScannerState(
    val lexeme: String,
    val source: String,
    val line: Int
):
  def uLex(lexeme: String) = this.copy(lexeme = lexeme)
  def uSrc(source: String) = this.copy(source = source)
  def uLin(line: Int) = this.copy(line = line)
