package dev.argon.js_module_extractor

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object JSModuleExtractor {

  @JSExportTopLevel("extractModuleFunctions")
  def usingJSObject(module: String): js.Dictionary[String] =
    usingScalaMap(module).toJSDictionary

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Equals"))
  def usingScalaMap(module: String): Map[String, String] = {

    val mod = Acorn.parse(module, AcornParserOptions(ecmaVersion = 11, sourceType = "module")).asInstanceOf[js.Dynamic]

    mod.body.asInstanceOf[js.Array[js.Dynamic]]
      .toSeq
      .filter { _.`type`.asInstanceOf[String] == "ExportNamedDeclaration" }
      .map { _.declaration }
      .filter { _.`type`.asInstanceOf[String] == "FunctionDeclaration" }
      .map { declaration =>
        declaration.id.name.asInstanceOf[String] -> module.substring(declaration.start.asInstanceOf[Int], declaration.end.asInstanceOf[Int])
      }
      .toMap
  }

}
