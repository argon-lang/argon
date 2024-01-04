package dev.argon.esexpr.generator

trait JavaGeneratorNaming extends GeneratorNaming {
  override def determineTypeName(name: String): String =
    GeneratorNaming.determineNamePascalCase(name)

  override def determineValueName(name: String): String =
    val formattedName = GeneratorNaming.determineNameCamelCase(name)
    if javax.lang.model.SourceVersion.isKeyword(formattedName) then
      "_" + formattedName
    else
      formattedName
  end determineValueName

  def getEnumCaseName(name: String): String =
    name.split("-").nn
      .map(_.nn.toUpperCase.nn)
      .mkString("_")
}

object JavaGeneratorNaming extends JavaGeneratorNaming
