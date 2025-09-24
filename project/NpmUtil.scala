import sbt._
import sbt.Keys._
import _root_.io.circe._
import _root_.io.circe.parser._
import org.scalajs.linker.interface.ModuleKind

import scala.sys.process.Process
import scala.util.control.NonFatal
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{fastLinkJS, fullLinkJS, scalaJSLinkerConfig, scalaJSLinkerOutputDirectory}

object NpmUtil extends AutoPlugin {

  object autoImport {
    val npmInstall = taskKey[Unit]("Runs npm install")
    val npmPackageLockJsonFile = settingKey[File]("The package-lock.json file")
    val npmDependencies = settingKey[Seq[(String, String)]]("NPM dependencies")
    val npmDevDependencies = settingKey[Seq[(String, String)]]("NPM dev dependencies")
  }
  import autoImport._

  def npmInstallCommon
  (
    name: String,
    packageLock: File,
    dir: File,
    linkerConfig: org.scalajs.linker.interface.StandardConfig,
    npmDependencies: Seq[(String, String)],
    npmDevDependencies: Seq[(String, String)],
  ): Unit = {

    val packageLockDest = dir / "package-lock.json"
    val packageJson = dir / "package.json"

    val newJson = Json.obj(
      "name" -> Json.fromString(name),
      "private" -> Json.fromBoolean(true),
      "type" -> Json.fromString(linkerConfig.moduleKind match {
        case ModuleKind.CommonJSModule => "commonjs"
        case ModuleKind.ESModule => "module"
        case _ => throw new Exception("Unexpected module type")
      }),
      "dependencies" -> Json.obj(
        npmDependencies.map { case (k, v) => k -> Json.fromString(v) }: _*
      ),
      "devDependencies" -> Json.obj(
        npmDevDependencies.map { case (k, v) => k -> Json.fromString(v) }: _*
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
      IO.createDirectory(dir)
      if(needsUpdate) IO.write(packageJson, newJson.spaces2)
      if(packageLock.exists()) IO.copyFile(packageLock, packageLockDest)
      Process("npm" :: "install" :: Nil, dir).!
      IO.copyFile(packageLockDest, packageLock)
    }
  }

  private def npmInstallTaskSetting
  (
    configuration: sbt.librarymanagement.Configuration,
    task: TaskKey[Unit],
    linkTask: TaskKey[Attributed[org.scalajs.linker.interface.Report]],
  ): Def.Setting[Task[Unit]] =
    configuration / task := Def.task {
      npmInstallCommon(
        name = name.value,
        packageLock = npmPackageLockJsonFile.value,
        dir = crossTarget.value,
        linkerConfig = scalaJSLinkerConfig.value,
        npmDependencies = npmDependencies.value,
        npmDevDependencies = npmDevDependencies.value,
      )
    }.value

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    npmPackageLockJsonFile := baseDirectory.value / "package-lock.json",
    npmDependencies := Seq(),
    npmDevDependencies := Seq(),

    npmInstallTaskSetting(Compile, npmInstall, fastLinkJS),
    Compile / compile := (Compile / compile).dependsOn(Compile / npmInstall).value,
  )
}

