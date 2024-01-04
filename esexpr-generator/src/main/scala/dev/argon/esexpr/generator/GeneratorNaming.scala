package dev.argon.esexpr.generator

import org.apache.commons.lang3.StringUtils

trait GeneratorNaming {
  def determineTypeName(name: String): String
  def determineValueName(name: String): String
}

object GeneratorNaming {


  def determineNamePascalCase(name: String): String =
    name.split("-").nn
      .map(StringUtils.capitalize)
      .mkString


  def determineNameCamelCase(name: String): String =
    StringUtils.uncapitalize(
      determineNamePascalCase(name)
    ).nn
}
