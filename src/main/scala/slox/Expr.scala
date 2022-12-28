package com.craftinginterpreters.slox

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(Expr: experession) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
