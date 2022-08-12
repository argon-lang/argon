package dev.argon.plugins.js

import ModuleResolution.*
import zio.json.JsonDecoder
import zio.json.ast.{Json, JsonCursor}

import java.util.Locale

final class ModuleResolution(fileSystem: Map[String, String]) {

  val defaultConditions: Seq[String] = Seq("import")

  def resolveSpecifier(specifier: String, referencingModulePath: String): String =
    esmResolve(specifier, dirname(referencingModulePath))


  private def dirname(path: String): String =
    val index = path.lastIndexOf('/')
    if index > 0 then // Use > instead of >= to ensure that /file.txt results in /.
      path.substring(0, index).nn
    else
      "/"
  end dirname

  private def join(basePath: String, otherPath: String): String =
    if otherPath.startsWith("/") then
      otherPath
    else if otherPath.startsWith("./") then
      join(basePath, otherPath.substring(2).nn)
    else if otherPath.startsWith("../") then
      join(dirname(basePath), otherPath.substring(3).nn)
    else
      val index = otherPath.indexOf('/')
      if index >= 0 then
        join(basePath + "/" + otherPath.substring(0, index).nn, otherPath.substring(index + 1).nn)
      else
        s"$basePath/$otherPath"
    end if

  private def esmResolve(specifier: String, parentURL: String): String =
    val resolved =
      if specifier.startsWith("./") || specifier.startsWith("../") || specifier.startsWith("/") then
        join(parentURL, specifier)
      else if specifier.startsWith("#") then
        packageImportsResolve(specifier, parentURL, defaultConditions)
      else
        packageResolve(specifier, parentURL)

    if resolved.contains("%2F") || resolved.contains("%2f") || resolved.contains("%5C") || resolved.contains("%5c") then
      throw InvalidModuleSpecifierException(resolved)

    if fileSystem.keys.exists(_.startsWith(resolved + "/")) then
      throw UnsupportedDirectoryImportException(resolved)

    if !fileSystem.contains(resolved) then
      throw ModuleNotFoundException(resolved)

    resolved
  end esmResolve

  private def packageResolve(packageSpecifier: String, parentURL: String): String =
    val packageName =
      if packageSpecifier.isEmpty then
        throw InvalidModuleSpecifierException(packageSpecifier)
      else if !packageSpecifier.startsWith("@") then
        val index = packageSpecifier.indexOf('/')
        if index >= 0 then
          packageSpecifier.substring(0, index).nn
        else
          packageSpecifier
      else
        val index = packageSpecifier.indexOf('/')
        if index >= 0 then
          val index2 = packageSpecifier.indexOf('/', index + 1)
          if index2 >= 0 then
            packageSpecifier.substring(0, index2).nn
          else
            packageSpecifier
        else
          throw InvalidModuleSpecifierException(packageSpecifier)
        end if
      end if

    if packageName.startsWith(".") || packageName.contains("\\") || packageName.contains("%") then
      throw InvalidModuleSpecifierException(packageSpecifier)



    val packageSubpath = "." + packageSpecifier.substring(packageName.length)

    if packageSubpath.endsWith("/") then
      throw InvalidModuleSpecifierException(packageSpecifier)


    packageSelfResolve(packageName, packageSubpath, parentURL).getOrElse {
      def loop(parentURL: String): String =
        if parentURL == "/" then
          throw ModuleNotFoundException(packageSpecifier)
        else
          val packageURL = join(parentURL, "node_modules/" + packageName)
          readPackageJson(packageURL)
            .map { pjson =>
              def packageExportPath: Option[String] =
                pjson.get(JsonCursor.isObject.field("exports"))
                  .toOption
                  .filterNot { _.isInstanceOf[Json.Null.type] }
                  .map { pjsonExports => packageExportsResolve(packageURL, packageSubpath, pjsonExports, defaultConditions) }

              def packageMain: Option[String] =
                if packageSubpath == "." then
                  pjson.get(JsonCursor.isObject.field("main").isString)
                    .map { _.value }
                    .toOption
                else
                  None

              packageExportPath.orElse(packageMain).getOrElse(join(packageURL, packageSubpath))
            }
            .getOrElse(loop(dirname(parentURL)))
        end if
      end loop

      loop(parentURL.stripSuffix("/"))
    }
  end packageResolve

  private def packageSelfResolve(packageName: String, packageSubpath: String, parentURL: String): Option[String] =
    for
      packageURL <- lookupPackageScope(parentURL)
      pjson <- readPackageJson(packageURL)
      pjsonName <- pjson.get(JsonCursor.isObject.field("name").isString).toOption if pjsonName.value == packageName
      pjsonExports <- pjson.get(JsonCursor.isObject.field("exports")).toOption if !pjsonExports.isInstanceOf[Json.Null.type]
    yield packageExportsResolve(packageURL, packageSubpath, pjsonExports, defaultConditions)

  def packageExportsResolve(packageURL: String, subpath: String, exports: Json, conditions: Seq[String]): String =
    val exportsWithDotKey = exports match {
      case obj @ Json.Obj(fields) if fields.exists { _._1.startsWith(".") } => Some(obj)
      case _ => None
    }
    val exportsWithNonDotKey = exports match {
      case obj @ Json.Obj(fields) if fields.exists { !_._1.startsWith(".") } => Some(obj)
      case _ => None
    }

    if exportsWithDotKey.isDefined && exportsWithNonDotKey.isDefined then
      throw InvalidPackageConfigurationException(packageURL)

    if subpath == "." then
      exports.get(JsonCursor.isString).toOption
        .orElse(exports.get(JsonCursor.isArray).toOption)
        .orElse(exports.get(JsonCursor.isObject).toOption.filter { _ => exportsWithDotKey.isEmpty })
        .orElse(exports.get(JsonCursor.isObject.field(".")).toOption)
        .flatMap { mainExport => packageTargetResolve(packageURL, mainExport, "", false, false, conditions) }
        .getOrElse { throw PackagePathNotExportedException(packageURL, ".") }
    else
      exportsWithDotKey
        .flatMap { exports =>
          packageImportsExportsResolve(subpath, exports, packageURL, false, conditions)
        }
        .getOrElse {
          throw PackagePathNotExportedException(packageURL, subpath)
        }
    end if
  end packageExportsResolve

  private def packageImportsResolve(specifier: String, parentURL: String, conditions: Seq[String]): String =
    if !specifier.startsWith("#") || specifier == "#" || specifier.startsWith("#/") then
      throw InvalidModuleSpecifierException(specifier)

    (
      for
        packageURL <- lookupPackageScope(parentURL)
        pjson <- readPackageJson(packageURL)
        pjsonImports <- pjson.get(JsonCursor.isObject.field("imports").isObject).toOption
        resolved <- packageImportsExportsResolve(specifier, pjsonImports, packageURL, true, conditions)
      yield resolved
    ).getOrElse { throw PackageImportNotDefinedException(specifier) }

  private def packageImportsExportsResolve(matchKey: String, matchObj: Json.Obj, packageURL: String, isImports: Boolean, conditions: Seq[String]): Option[String] =
    (
      if matchKey.contains("*") then
        None
      else
        matchObj.get(JsonCursor.isObject.field(matchKey))
          .map { target => packageTargetResolve(packageURL, target, "", false, isImports, conditions) }
          .toOption
    ).getOrElse {
      def keyHasSingleStar(key: String): Boolean =
        val first = key.indexOf('*')
        first >= 0 && key.indexOf('*', first + 1) < 0
      end keyHasSingleStar

      def patternKeyLessThan(keyA: String, keyB: String): Boolean =
        val baseLengthA =
          val index = keyA.indexOf('*')
          if index >= 0 then index + 1
          else keyA.length
        end baseLengthA

        val baseLengthB =
          val index = keyB.indexOf('*')
          if index >= 0 then index + 1
          else keyB.length
        end baseLengthB

        if baseLengthA > baseLengthB then
          true
        else if baseLengthB > baseLengthA then
          false
        else if baseLengthA == keyA.length then
          false
        else if baseLengthB == keyB.length then
          true
        else if keyA.length > keyB.length then
          true
        else
          false
      end patternKeyLessThan

      matchObj.fields
        .filter { case (k, _) => keyHasSingleStar(k) }
        .sortWith { case ((k1, _), (k2, _)) => patternKeyLessThan(k1, k2) }
        .collectFirst(({ (expansionKey, target) =>
          val starIndex = expansionKey.indexOf('*')
          val patternBase = expansionKey.substring(0, starIndex).nn
          if matchKey.startsWith(patternBase) && matchKey != patternBase then
            val patternTrailer = expansionKey.substring(starIndex + 1)
            if matchKey.endsWith(patternTrailer) && matchKey.length >= expansionKey.length then
              val subpath = matchKey.substring(starIndex, matchKey.length - expansionKey.length).nn
              packageTargetResolve(packageURL, target, subpath, true, isImports, conditions)
            else
              None
            end if
          else
            None
          end if
        } : ((String, Json)) => Option[String]).unlift)
    }

  def packageTargetResolve(packageURL: String, target0: Json, subpath: String, pattern: Boolean, internal: Boolean, conditions: Seq[String]): Option[String] =
    target0 match {
      case Json.Str(target) =>
        if !pattern && subpath.nonEmpty && !target.endsWith("/") then
          throw InvalidModuleSpecifierException(subpath)

        val result =
          if target.startsWith("./") then
            def isInvalidSeg(seg: String): Boolean =
              seg == "." || seg == ".." || seg.toLowerCase(Locale.US).nn == "node_modules"

            if target.split(Array('/', '\\')).drop(1).exists(isInvalidSeg) then
              throw InvalidPackageTargetException(target0)

            val resolvedTarget = join(packageURL, target)

            if subpath.split(Array('/', '\\')).exists(isInvalidSeg) then
              throw InvalidModuleSpecifierException(subpath)

            if pattern then
              resolvedTarget.replace("*", subpath).nn
            else
              join(subpath, resolvedTarget)

          else
            if internal && !target.startsWith("../") && !target.startsWith("/") then
              if pattern then
                packageResolve(target.replace("*", subpath).nn, packageURL + "/")
              else
                packageResolve(target + subpath, packageURL + "/")
            else
              throw InvalidPackageTargetException(target0)
          end if

        Some(result)

      case Json.Obj(fields) =>
        fields
          .iterator
          .filter { (key, _) => key == "default" || conditions.contains(key) }
          .collectFirst((((key, value) =>
            packageTargetResolve(packageURL, value, subpath, pattern, internal, conditions)
          ) : ((String, Json)) => Option[String]).unlift)

      case Json.Arr(elements) =>
        elements.collectFirst(((targetValue =>
          packageTargetResolve(packageURL, targetValue, subpath, pattern, internal, conditions)
        ) : Json => Option[String]).unlift)

      case _: Json.Null.type =>
        None

      case _ =>
        throw InvalidPackageTargetException(target0)
    }

  private def lookupPackageScope(url: String): Option[String] =
    if url == "/" then
      None
    else
      val scopeURL = dirname(url)
      if scopeURL.endsWith("/node_modules") then
        None
      else if fileSystem.contains(join(scopeURL, "package.json")) then
        Some(scopeURL)
      else
        None
    end if

  private def readPackageJson(packageURL: String): Option[Json] =
    fileSystem.get(join(packageURL, "package.json"))
      .map { jsonText =>
        summon[JsonDecoder[Json]].decodeJson(jsonText)
          .getOrElse { throw InvalidPackageConfigurationException(packageURL) }
      }




}

object ModuleResolution {
  final class InvalidModuleSpecifierException(specifier: String) extends Exception(s"Invalid specifier: $specifier")
  final class ModuleNotFoundException(path: String) extends Exception(s"Could not find module: $path")
  final class UnsupportedDirectoryImportException(path: String) extends Exception
  final class InvalidPackageConfigurationException(packageURL: String) extends Exception
  final class PackagePathNotExportedException(packageURL: String, subpath: String) extends Exception(s"Module $packageURL does not export path $subpath")
  final class PackageImportNotDefinedException(specifier: String) extends Exception
  final class InvalidPackageTargetException(target: Json) extends Exception
}

