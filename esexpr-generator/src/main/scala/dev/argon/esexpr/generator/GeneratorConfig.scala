package dev.argon.esexpr.generator

import dev.argon.esexpr.{ESExprCodec, keyword}


enum GeneratorConfig derives ESExprCodec {
  val esxFile: String

  case Java(
    @keyword esxFile: String,
    @keyword outDir: String,
    @keyword packageName: String,
  )

  case Scala(
    @keyword esxFile: String,
    @keyword outFile: String,
    @keyword packageName: String,
  )

  case TypeScript(
    @keyword esxFile: String,
    @keyword outFile: String,
  )



  case JavaRunJS(
    @keyword esxFile: String,
    @keyword outDir: String,
    @keyword packageName: String,
    @keyword javaApiPackageName: String,
    @keyword jsUtilPackageName: String,
  )

  case ScalaJS(
    @keyword esxFile: String,
    @keyword outFile: String,
    @keyword packageName: String,
  )

  case ScalaRunJava(
    @keyword esxFile: String,
    @keyword outFile: String,
    @keyword packageName: String,
    @keyword scalaApiPackageName: String,
    @keyword javaApiPackageName: String,
    @keyword utilPackageName: String,
  )

  case ScalaRunSJS(
    @keyword esxFile: String,
    @keyword outFile: String,
    @keyword packageName: String,
    @keyword scalaApiPackageName: String,
    @keyword jsApiPackageName: String,
    @keyword utilPackageName: String,
  )
}


