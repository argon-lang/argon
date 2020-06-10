package dev.argon.js_module_extractor

import dev.argon.js_module_extractor.AcornParserOptions.{AllowedEcmaVersions, SourceType}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.|

@js.native
@JSImport("acorn", JSImport.Namespace)
object Acorn extends js.Object {

  def parse(code: String, parserOptions: AcornParserOptions): js.Object = js.native

}

trait AcornParserOptions extends js.Object {
  val ecmaVersion: js.UndefOr[AllowedEcmaVersions]
  val sourceType: js.UndefOr[SourceType]
}

object AcornParserOptions {

  type AllowedEcmaVersions = Int
  type SourceType = String

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def apply
  (
    ecmaVersion: js.UndefOr[AllowedEcmaVersions] = js.undefined,
    sourceType: js.UndefOr[SourceType] = js.undefined,
  ): AcornParserOptions =
    js.Dynamic.literal(
      ecmaVersion = ecmaVersion,
      sourceType = sourceType,
    ).asInstanceOf[AcornParserOptions]

}

