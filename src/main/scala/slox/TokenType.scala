package com.craftinginterpreters.lox

sealed trait TokenType

enum Single extends TokenType:
  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR

sealed trait OneOrTwo extends TokenType
// One or two character tokens.
enum One extends OneOrTwo:
  case BANG, EQUAL, GREATER, LESS
enum Two extends OneOrTwo:
  case BANG_EQUAL, EQUAL_EQUAL, GREATER_EQUAL, LESS_EQUAL

enum Literal extends TokenType:
  // Literals.
  case IDENTIFIER, STRING, NUMBER

enum Keyword extends TokenType:
  // Keywords.
  case AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
    EOF
