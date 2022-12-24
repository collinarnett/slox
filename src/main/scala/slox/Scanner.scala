package com.craftinginterpreters.lox
import Single.*
import One.*
import Two.*
import Literal.*
import Keyword.*

case class Scanner(val src: String):
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
            val (head, tail) = state.source.span(_ != '\n')
            (None, state.uSrc(state.source.tail))
          case None => addToken(SLASH, state.uSrc(""))
          case _    => addToken(SLASH, state)
      case ' ' | '\r' | '\t' => (None, state)
      case '\n' =>
        (None, ScannerState("", state.source, state.line + 1))
      case '"' =>
        val (head, tail) = state.source.span(_ != '"')
        if (tail.isEmpty())
          Slox().error(state.line, "Unterminated string.")
          (None, state)
        else
          (
            Some(Token(STRING, head, head, state.line)),
            state
              .uSrc(tail.tail)
              .uLin(state.line + head.count(_ == '\n'))
          )
      case _ => (None, state)
  private def addToken(
      _type: TokenType,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    (Some(Token(_type, state.lexeme, None, state.line)), state)
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

case class ScannerState(
    val lexeme: String,
    val source: String,
    val line: Int
):
  def uLex(lexeme: String) = this.copy(lexeme = lexeme)
  def uSrc(source: String) = this.copy(source = source)
  def uLin(line: Int) = this.copy(line = line)
