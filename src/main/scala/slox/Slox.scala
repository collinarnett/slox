package com.craftinginterpreters.slox

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import java.util.List

case class Slox():
  def error(line: Int, message: String) =
    report(line, "", message)

  def runFile(path: String) =
    val bytes = Files.readAllBytes(Paths.get(path))
    run(String(bytes, Charset.defaultCharset))

  def runPrompt =
    val input = InputStreamReader(System.in)
    val reader = BufferedReader(input)

    while true do
      print("> ")
      val line: String = reader.readLine()
      line match
        case l: String => run(l)
        case null      => System.exit(65)

  private def run(source: String) =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()

    for token <- tokens.tokens do println(token)

  private def report(line: Int, where: String, message: String) =
    System.err.println(
      s"[line $line] Error $where: $message"
    )

object Slox:
  def main(args: Array[String]) =
    if (args.length > 1)
      println("Usage: jlox [script]")
    else if (args.length == 1)
      Slox().runFile(args.head)
    else
      Slox().runPrompt
