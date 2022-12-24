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
    if (state.source.isEmpty()) then return tokens
    val (tokenOption: Option[Token], _state: ScannerState) = scanToken(
      state.source.head,
      ScannerState(
        state.source.head.toString(),
        state.source.tail,
        state.line
      )
    )
    tokenOption match
      case Some(token) => scanTokens(_state, tokens :+ token)
      case _           => scanTokens(_state, tokens)
  private def scanToken(
      c: Char,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    c match
      case '(' => addSingle(LEFT_PAREN, state)
      case ')' => addSingle(RIGHT_PAREN, state)
      case '{' => addSingle(LEFT_BRACE, state)
      case '}' => addSingle(RIGHT_BRACE, state)
      case ',' => addSingle(COMMA, state)
      case '.' => addSingle(DOT, state)
      case '-' => addSingle(MINUS, state)
      case '+' => addSingle(PLUS, state)
      case ';' => addSingle(SEMICOLON, state)
      case '*' => addSingle(STAR, state)
      case '!' => addCouple(BANG, BANG_EQUAL, state)
      case '=' => addCouple(EQUAL, EQUAL_EQUAL, state)
      case '<' => addCouple(LESS, LESS_EQUAL, state)
      case '>' => addCouple(GREATER, GREATER_EQUAL, state)
      case '/' =>
        state.source.head match
          case '/' =>
            val (head, tail) = state.source.span(_ != '\n')
            (None, ScannerState("", state.source.tail, state.line))
          case _ => addSingle(SLASH, state)
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
            ScannerState(
              "",
              state.source.tail,
              state.line + head.count(_ == '\n')
            )
          )
      case _ => (None, state)
  private def addSingle(
      _type: TokenType,
      state: ScannerState
  ): (Option[Token], ScannerState) =
    (Some(Token(_type, state.lexeme, None, state.line)), state)
  private def addCouple(
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
          ScannerState("", source.tail, line)
        )
      case _ => (Some(Token(op1, state.lexeme, None, line)), state)
case class ScannerState(
    val lexeme: String,
    val source: String,
    val line: Int
)
