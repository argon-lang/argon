import sbt._
import sbt.Keys._
import _root_.io.circe._
import _root_.io.circe.parser._
import org.scalajs.linker.interface.ModuleKind

import scala.sys.process.Process
import scala.util.control.NonFatal
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSLinkerConfig

object NpmUtil extends AutoPlugin {

  object autoImport {
    val npmInstall = taskKey[Unit]("Runs npm install")


    val npmPackageLockJsonFile = settingKey[File]("The package-lock.json file")
    val npmDependencies = settingKey[Seq[(String, String)]]("NPM packages")
    val npmDevDependencies = settingKey[Seq[(String, String)]]("NPM dev packages")
  }
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    npmPackageLockJsonFile := baseDirectory.value / "package-lock.json",
    npmDependencies := Seq(),
    npmDevDependencies := Seq(),
    npmInstall := {
      val packageLock = npmPackageLockJsonFile.value

      val dir = crossTarget.value

      val linkerConfig = scalaJSLinkerConfig.value

      val packageLockDest = dir / "package-lock.json"
      val packageJson = dir / "package.json"

      val newJson = Json.obj(
        "name" -> Json.fromString(name.value),
        "private" -> Json.fromBoolean(true),
        "type" -> Json.fromString(linkerConfig.moduleKind match {
          case ModuleKind.CommonJSModule => "commonjs"
          case ModuleKind.ESModule => "module"
          case _ => throw new Exception("Unexpected module type")
        }),
        "dependencies" -> Json.obj(
          npmDependencies.value.map { case (k, v) => k -> Json.fromString(v) }: _*
        ),
        "devDependencies" -> Json.obj(
          npmDevDependencies.value.map { case (k, v) => k -> Json.fromString(v) }: _*
        ),
      )

      val needsUpdate =
        try {
          parse(IO.read(packageJson)) match {
            case Left(_) => true
            case Right(currentPackages) => currentPackages != newJson
          }
        }
        catch { case NonFatal(_) => true }


      if(needsUpdate || !(dir / "node_modules").exists()) {
        if(needsUpdate) IO.write(packageJson, newJson.spaces2)
        if(packageLock.exists()) IO.copyFile(packageLock, packageLockDest)
        Process("npm" :: "install" :: Nil, dir).!
        IO.copyFile(packageLockDest, packageLock)
      }
    },

    Compile / run := (Compile / run).dependsOn(npmInstall).evaluated,
    Test / test := (Test / test).dependsOn(npmInstall).value,
  )
}

