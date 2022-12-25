package com.craftinginterpreters.lox

case class Token(
    val _type: TokenType,
    val lexeme: String,
    val literal: Any,
    val line: Int
):
  override def toString() =
    s"$lexeme"

case class Tokens(
    val tokens: Seq[Token]
):
  def :+(token: Token): Tokens =
    this.copy(this.tokens :+ token)
