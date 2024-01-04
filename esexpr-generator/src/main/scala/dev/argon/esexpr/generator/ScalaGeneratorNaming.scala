package dev.argon.esexpr.generator

trait ScalaGeneratorNaming extends GeneratorNaming {
  private val keywords = Seq(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
  )

  override def determineTypeName(name: String): String =
    GeneratorNaming.determineNamePascalCase(name)

  override def determineValueName(name: String): String =
    escapeName(GeneratorNaming.determineNameCamelCase(name))
  
  def determineJavaValueName(name: String): String =
    escapeName(JavaGeneratorNaming.determineValueName(name))

  private def escapeName(name: String): String =
    if keywords.contains(name) then
      s"`$name`"
    else
      name

}

object ScalaGeneratorNaming extends ScalaGeneratorNaming
