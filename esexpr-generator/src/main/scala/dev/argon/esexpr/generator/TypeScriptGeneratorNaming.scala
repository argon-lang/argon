package dev.argon.esexpr.generator

trait TypeScriptGeneratorNaming extends GeneratorNaming {
  override def determineTypeName(name: String): String =
    GeneratorNaming.determineNamePascalCase(name)

  override def determineValueName(name: String): String =
    GeneratorNaming.determineNameCamelCase(name)
}

object TypeScriptGeneratorNaming extends TypeScriptGeneratorNaming
