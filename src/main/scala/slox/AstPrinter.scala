package com.craftinginterpreters.slox

case class AstPrinter():
  def print(expr: Expr): String = expr match
    case x: Binary   => parenthesize(x.operator.lexeme, x.left, x.right)
    case x: Grouping => parenthesize("group", x.expression)
    case x: Literal  => x.value.toString
    case x: Unary    => parenthesize(x.operator.lexeme, x.right)

  private def parenthesize(name: String, exprs: Expr*) =
    val expression = exprs.map(expr => s" ${AstPrinter().print(expr)}").mkString
    s"($name$expression)"
