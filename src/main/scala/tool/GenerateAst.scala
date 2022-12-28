package com.craftinginterpreters.tool
import java.io.PrintWriter

case class Field(value: String, _type: String):
  override def toString() =
    s"$value: $_type"

case class GenerateAst():
  def defineAst(
      outputDir: String,
      baseName: String,
      types: Map[String, Seq[Field]]
  ) =
    val path = s"$outputDir/$baseName.scala"
    val writer = PrintWriter(path, "UTF-8")
    writer.println("package com.craftinginterpreters.slox")
    writer.println
    writer.println(s"sealed trait $baseName")
    // The AST classes.
    for (className, fields) <- types do
      defineType(writer, baseName, className, fields)
    writer.close

  private def defineType(
      writer: PrintWriter,
      baseName: String,
      className: String,
      fieldList: Seq[Field]
  ) =
    val fields = fieldList.mkString(", ")

    writer.println(s"case class $className($fields) extends $baseName")

object GenerateAst:
  def main(args: Array[String]) =
    val outputDirOption = args.headOption

    val types = Map[String, Seq[Field]](
      "Binary" -> Seq(
        Field("left", "Expr"),
        Field("operator", "Token"),
        Field("right", "Expr")
      ),
      "Grouping" -> Seq(
        Field("expression", "Expr")
      ),
      "Literal" -> Seq(
        Field("value", "Any")
      ),
      "Unary" -> Seq(
        Field("operator", "Token"),
        Field("right", "Expr")
      )
    )
    val astGenerator = GenerateAst()
    outputDirOption match
      case Some(outputDir) =>
        astGenerator.defineAst(
          outputDir,
          "Expr",
          types
        )
      case None =>
        System.err.println("Usage: generate_ast <output directory>")
        System.exit(64)
