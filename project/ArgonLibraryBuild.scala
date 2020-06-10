import sbt.Keys.streams
import sbt.{Path, Run}
import sbt._

object ArgonLibraryBuild {

  def eachCommand(libName: String)(f: (String, Seq[String]) => Unit): Unit = {

    val libDir = file("libraries") / libName

    val jsExternFile = libDir / "js/extern.js"
    val jsInjectBeforeFile = libDir / "js/inject_before.js"
    val jsInjectAfterFile = libDir / "js/inject_after.js"

    val backends = Seq(
      "argon-module" -> Seq(
        "--argon-module:referenceModule",
        (libDir / "bin" / (libName + ".armodule")).toString,
      ),
      "js" -> (
        Seq(
          "--js:outputFile",
          (libDir / "bin/js" / (libName + ".js")).toString,
        ) ++
          (if(jsExternFile.exists()) Seq("--js:extern", jsExternFile.toString) else Seq.empty) ++
          (if(jsInjectBeforeFile.exists()) Seq("--js:inject.before", jsInjectBeforeFile.toString) else Seq.empty) ++
          (if(jsInjectAfterFile.exists()) Seq("--js:inject.after", jsInjectAfterFile.toString) else Seq.empty)
        )
    )

    val inputFileOpts =
      Path.allSubpaths(libDir / "src")
        .map(_._1.toString)
        .filter(_.endsWith(".argon"))
        .flatMap { file => Seq("--inputFiles", file) }

    backends.foreach { case (backend, opts) =>
      val commandArgs =
        Seq(
          "build",
          backend,

          "--moduleName",
          libName,
        ) ++
          inputFileOpts ++
          opts

      f(backend, commandArgs)
    }

  }

}
