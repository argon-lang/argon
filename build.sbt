import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits.*
import org.scalajs.linker.interface.ESVersion
import complete.DefaultParsers.*
import java.util.Locale

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}


val JSBackendTag = Tags.Tag("argon-js-backend")


ThisBuild / resolvers += Resolver.mavenLocal
Global / semanticdbEnabled := true
Global / concurrentRestrictions += Tags.limit(JSBackendTag, 1)

val graalVersion = "25.0.0"
val zioVersion = "2.1.21"

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.7.3",
  Compile / run / baseDirectory := file(".").getAbsoluteFile,
  Test / baseDirectory := file(".").getAbsoluteFile,
)



lazy val dist = taskKey[Unit]("Builds the distributions of the compiler")
lazy val distJVM = taskKey[Unit]("Builds the JVM distribution of the compiler")
lazy val distNode = taskKey[Unit]("Builds the Node distribution of the compiler")
lazy val distBackends = taskKey[Unit]("Builds the backend distributions of the compiler")
lazy val distBackendJS = taskKey[Unit]("Builds the JS backend distribution of the compiler")

dist := {
  val _1: Unit = distJVM.value
  val _2: Unit = distNode.value
}

distJVM := {
  val distDir = file("dist/argon-jvm")
  IO.delete(distDir)

  val compilerJarDir = distDir / "compiler"
  IO.createDirectory(compilerJarDir)

  val files = (compiler_launcher / Compile / fullClasspathAsJars)
    .value
    .map { _.data }

  for (file <- files) {
    IO.copyFile(file, compilerJarDir / file.getName)
  }

  val _ = distBackendJS.value
  IO.createDirectory(distDir / "backends/js")
  IO.copyDirectory(file("dist/backends/js"), distDir / "backends/js")

  val launcherScript = distDir / "argon"
  IO.write(
    launcherScript,
    s"""#!/bin/bash
       |DISTDIR="$$(readlink -f "$$(dirname "$$0")")"
       |COMPDIR="$$DISTDIR/compiler"
       |exec java \\
       |  --class-path "${ files.map(file => "$COMPDIR/" + file.getName).mkString(":") }" \\
       |  -Ddev.argon.backends="$$DISTDIR/backends" \\
       |  --illegal-native-access=deny \\
       |  -Dpolyglotimpl.AttachLibraryFailureAction=ignore \\
       |  dev.argon.launcher.ArgonLauncher \\
       |  "$$@"
       |""".stripMargin
  )
  launcherScript.setExecutable(true)
}

distNode := {
  val s = streams.value
  val log = s.log

  val procLog = new ProcessLogger {
    override def out(s: => String): Unit = log.out(s)
    override def err(s: => String): Unit = log.out(s)
    override def buffer[T](f: => T): T = f
  }


  val distDir = file("dist/argon-js")
  IO.delete(distDir)

  val compilerPackageDir = distDir / "compiler"
  IO.createDirectory(compilerPackageDir)


  def npmInstall(dir: File): Unit = {
    log.info("Installing npm dependencies in " + dir)
    val exitCode = Process(Seq("npm", "install"), Some(dir)) ! procLog
    if(exitCode != 0) {
      throw new Exception("npm install failed with exit code " + exitCode)
    }
  }
  def npmRun(dir: File, command: String): Unit = {
    log.info("Running npm run build in " + dir)
    val exitCode = Process(Seq("npm", "run", command), dir) ! procLog
    if(exitCode != 0) {
      throw new Exception(s"npm run $command failed with exit code " + exitCode)
    }
  }
  def packageCopy(src: File, dest: File): Unit = {
    val exitCode = Process(
      Seq("node", "lib/main.js", src.getAbsolutePath, dest.getAbsolutePath),
      file("backend/util/js-copy-deploy"),
    ) ! procLog
    if(exitCode != 0) {
      throw new Exception("package copy with exit code " + exitCode)
    }
  }

  val launcherDir = file("argon-compiler-launcher-js")

  npmInstall(launcherDir)
  npmRun(launcherDir, "build")

  val compilerDriverOutput = (compiler_driverNode / Compile / fullOptJS).value
  packageCopy(file("argon-compiler-launcher-js"), compilerPackageDir)
  IO.copyFile(compilerDriverOutput.data, compilerPackageDir / "lib/argon.js")


  val _ = distBackends.value
  IO.copyDirectory(file("dist/backends"), distDir / "backends")

  val launcherScript = distDir / "argon"
  IO.write(
    launcherScript,
    s"""#!/bin/bash
       |DISTDIR="$$(readlink -f "$$(dirname "$$0")")"
       |COMPDIR="$$DISTDIR/compiler"
       |exec node \\
       |  "$$COMPDIR/lib/index.js" \\
       |  "$$@"
       |""".stripMargin
  )
  launcherScript.setExecutable(true)
}

distBackends := {
  val _: Unit = distBackendJS.value
}

distBackendJS := Def.task {
  val s = streams.value
  val log = s.log

  val procLog = new ProcessLogger {
    override def out(s: => String): Unit = log.out(s)
    override def err(s: => String): Unit = log.out(s)
    override def buffer[T](f: => T): T = f
  }

  def npmInstall(dir: File): Unit = {
    log.info("Installing npm dependencies in " + dir)
    val exitCode = Process(Seq("npm", "install"), Some(dir)) ! procLog
    if(exitCode != 0) {
      throw new Exception("npm install failed with exit code " + exitCode)
    }
  }
  def npmRun(dir: File, command: String): Unit = {
    log.info("Running npm run build in " + dir)
    val exitCode = Process(Seq("npm", "run", command), dir) ! procLog
    if(exitCode != 0) {
      throw new Exception(s"npm run $command failed with exit code " + exitCode)
    }
  }
  def packageCopy(src: File, dest: File): Unit = {
    val exitCode = Process(
      Seq("node", "lib/main.js", src.getAbsolutePath, dest.getAbsolutePath),
      file("backend/util/js-copy-deploy"),
    ) ! procLog
    if(exitCode != 0) {
      throw new Exception("package copy with exit code " + exitCode)
    }
  }

  val jsApiDir = file("backend/api/js")
  val jsBackendDir = file("backend/backends/js")

  npmInstall(jsApiDir)
  npmRun(jsApiDir, "build")
  npmInstall(jsBackendDir)
  npmRun(jsBackendDir, "build")

  val distBackendDir = file("dist/backends/js")
  IO.delete(distBackendDir)

  packageCopy(jsBackendDir, distBackendDir)
}.tag(JSBackendTag).value




lazy val commonSettings = commonSettingsNoLibs ++ Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "dev.argon" %%% "argon-async-util" % "2.1.0",
    "dev.argon.esexpr" %%% "esexpr-scala-runtime" % "0.3.3",
    "dev.argon.nobleidl" %%% "nobleidl-scala-runtime" % "0.1.0-SNAPSHOT",

    "com.lihaoyi" %%% "sourcecode" % "0.4.4",

    "dev.optics" %%% "monocle-core"  % "3.3.0",
    "dev.optics" %%% "monocle-macro" % "3.3.0",

    "org.scala-lang.modules" %%% "scala-xml" % "2.4.0",
    "org.gnieh" %%% "fs2-data-xml-scala" % "1.12.0",
    "com.indoorvivants" %%% "toml" % "0.3.0",
    "io.kevinlee" %%% "just-semver-core" % "1.1.1",

    "org.typelevel" %%% "cats-core" % "2.13.0",
    "dev.zio" %%% "zio-interop-cats" % "23.1.0.5",
  ),
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "org.scala-lang" %% "scala3-library" % scalaVersion.value,
    "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  ),

  npmDependencies ++= Seq(
    "@argon-lang/esexpr" -> "^0.2.1",
    "@argon-lang/js-backend-api" -> "file:../../../../backend/api/js",
    "@argon-lang/compiler-backend-js" -> "file:../../../../backend/backends/js",
  ),
  
  scalaJSLinkerConfig ~= {
    _
      .withModuleKind(ModuleKind.ESModule)
      .withBatchMode(true)
      .withESFeatures(_
        .withESVersion(ESVersion.ES2021)
//        .allowBigIntsForLongs(true)
      )
  },

  fork := false,

)

lazy val annotationDependencies = Seq(
  "org.jetbrains" % "annotations" % "26.0.2",
)

lazy val commonJVMSettingsNoLibs = Seq(
  fork := true,
)

lazy val commonJVMSettings = commonJVMSettingsNoLibs ++ Seq(

  javaOptions ++= Seq(
    "--illegal-native-access=deny",
    "-Dpolyglotimpl.AttachLibraryFailureAction=ignore"
  ),


  libraryDependencies ++= annotationDependencies ++ Seq(
    "dev.zio" %% "zio-logging" % "2.5.1",

    "org.apache.commons" % "commons-compress" % "1.28.0",
    "org.apache.commons" % "commons-text" % "1.14.0",

    "org.graalvm.polyglot" % "polyglot" % graalVersion,
    "org.graalvm.polyglot" % "js-community" % graalVersion,
  ),

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val nodeEnvConfig =
  NodeJSEnv.Config()
    .withArgs(List("--no-warnings", "--experimental-vm-modules"))

lazy val commonNodeSettings = sharedJSNodeSettings ++ Seq(

  jsEnv := new NodeJSEnv(nodeEnvConfig),

  npmDevDependencies ++= Seq(
    "@types/node" -> "24.5.2",
  ),

  Compile / run := {
    val log = streams.value.log
    val jsFile = (Compile / fastOptJS).value.data
    val args = spaceDelimited("<arg>").parsed
    val cwd = (Compile / run / baseDirectory).value
    val env = (Compile / run / envVars).value

    val exitCode = Process("node" +: jsFile.getAbsolutePath +: args, cwd, env.toSeq*) ! log
    if (exitCode != 0) throw new RuntimeException(s"Process exited with code $exitCode")
  },
)

lazy val compilerOptions = Seq(

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-release", "24",
    "-source", "future",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:strictEquality",
    "-deprecation",
    "-feature",
    "-Ycheck-all-patmat",
    "-Yretain-trees",
    "-Yexplicit-nulls",
    "-Xmax-inlines", "128",
    "-Wconf:id=E029:e,id=E165:e,id=E190:e,cat=unchecked:e,cat=deprecation:e",
  ),

  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "--release", "24",
    "-Werror",
    "-Xlint:all,-serial,-try,-processing",
  ),

)


lazy val util = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-util"))
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.util",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    usePipelining := false,

    name := "argon-util",
  )

lazy val utilJVM = util.jvm
lazy val utilJS = util.js
lazy val utilNode = util.node

lazy val ast = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-ast"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.ast",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-ast",
  )

lazy val astJVM = ast.jvm
lazy val astJS = ast.js
lazy val astNode = ast.node


lazy val parser = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-parser"))
  .dependsOn(ast)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.parser",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,


    Compile / managedSourceDirectories += sourceManaged.value / "parser",
    Compile / sourceGenerators += Def.task {
      val s = streams.value
      val log = s.log

      val resDir = sourceManaged.value / "parser"

      val parserGenFiles = Seq(
        ("token_lexer", "TokenLexer"),
        ("double_quote_string_lexer", "StringLexer"),
        ("argon_parser", "ArgonParserImpl"),
      )


      parserGenFiles
        .flatMap { case (exeName, sourceFileName) =>
          val backendDestFile = resDir / s"dev/argon/parser/$sourceFileName.scala"
          val ext = if(System.getProperty("os.name", "").startsWith("Windows")) ".exe" else ""
          val exeFile = file(s"parse18/target/release/$exeName$ext")

          val cargoBuildErrors = ArrayBuffer[String]()
          val ignoreOutput = ProcessLogger(
            out => cargoBuildErrors += out,
            err => cargoBuildErrors += err,
          )

          val buildExitCode = Process(Seq("cargo", "build", "--bin", exeName, "--release"), Some(file("parse18"))) ! ignoreOutput
          if(buildExitCode != 0) {
            cargoBuildErrors.foreach(log.error(_))
            throw new Exception("cargo build failed with exit code " + buildExitCode)
          }

          val f = FileFunction.cached(s.cacheDirectory / ("parser-" + exeName)) { (in: Set[File]) =>
            log.info("Generating parser in " + backendDestFile)

            IO.createDirectory(backendDestFile.getParentFile)
            val genExitCode = Process(Seq(exeFile.toString)) #> backendDestFile ! log
            if(genExitCode != 0) {
              throw new Exception(s"Parser generation ($exeName) failed with exit code $genExitCode")
            }

            Set(backendDestFile)
          }

          f(Set(exeFile))
        }
        .toSeq
    }.taskValue,

    name := "argon-parser",
  )

lazy val parserJVM = parser.jvm
lazy val parserJS = parser.js
lazy val parserNode = parser.node


lazy val argon_prover = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-prover"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.prover",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-prover",
  )

lazy val argon_proverJVM = argon_prover.jvm
lazy val argon_proverJS = argon_prover.js
lazy val argon_proverNode = argon_prover.node


lazy val argon_io = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-io"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.io",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-io",
  )

lazy val argon_ioJVM = argon_io.jvm
lazy val argon_ioJS = argon_io.js
lazy val argon_ioNode = argon_io.node


lazy val argon_expr = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-expr"))
  .dependsOn(argon_prover, util, ast)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.expr",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-expr",
  )

lazy val argon_exprJVM = argon_expr.jvm
lazy val argon_exprJS = argon_expr.js
lazy val argon_exprNode = argon_expr.node


lazy val argon_compiler = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-compiler"))
  .dependsOn(ast, util, argon_expr, argon_io)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.compiler",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler",
  )

lazy val argon_compilerJVM = argon_compiler.jvm
lazy val argon_compilerJS = argon_compiler.js
lazy val argon_compilerNode = argon_compiler.node


lazy val argon_format = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-format"))
  .enablePlugins(NobleIDLPlugin)
  .dependsOn(argon_compiler)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,
      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.format",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    Compile / nobleIdlSourceDirectories += baseDirectory.value / "../../backend/format",

    name := "argon-format",
  )

lazy val argon_formatJVM = argon_format.jvm
lazy val argon_formatJS = argon_format.js
lazy val argon_formatNode = argon_format.node


lazy val argon_tube = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-tube"))
  .dependsOn(argon_format, argon_io, argon_compiler)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.tube",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-tube",
  )

lazy val argon_tubeJVM = argon_tube.jvm
lazy val argon_tubeJS = argon_tube.js
lazy val argon_tubeNode = argon_tube.node

lazy val argon_source = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-source"))
  .dependsOn(argon_compiler, parser)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.source",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-source",
  )

lazy val argon_sourceJVM = argon_source.jvm
lazy val argon_sourceJS = argon_source.js
lazy val argon_sourceNode = argon_source.node


lazy val argon_backend_java_api = project.in(file("backend/api/java"))
  .enablePlugins(NobleIDLPlugin)
  .settings(
    commonSettingsNoLibs,
    commonJVMSettingsNoLibs,
    compilerOptions,

    compileOrder := CompileOrder.JavaThenScala,

    semanticdbEnabled := false,
    autoScalaLibrary := false,
    crossPaths := false,

    Compile / javacOptions ++= {
      val modulePath = (Compile / dependencyClasspath).value.map(_.data.getAbsolutePath).mkString(java.io.File.pathSeparator)

      Seq(
        "--module-path", modulePath,
      )
    },

    libraryDependencies ++= Seq(
      "org.jetbrains" % "annotations" % "26.0.2",
      "dev.argon.nobleidl" % "nobleidl-java-runtime" % "0.1.0-SNAPSHOT",
      "dev.argon.esexpr" % "esexpr-java-runtime" % "0.3.1",
      "org.graalvm.polyglot" % "polyglot" % graalVersion,
      "org.graalvm.polyglot" % "js-community" % graalVersion,
    ),

    Compile / generateNobleIdlScala := false,
    Compile / generateNobleIdlJava := true,
    Compile / generateNobleIdlGraalJsAdapters := true,
    Compile / nobleIdlSourceDirectories ++= Seq(
      baseDirectory.value / "../../vm/",
      baseDirectory.value / "../nobleidl",
    ),
  )


lazy val argon_vm = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Full).in(file("argon-vm"))
  .enablePlugins(NobleIDLPlugin)
  .dependsOn(util, argon_compiler, argon_format, argon_tube)
  .jvmConfigure(
    _.dependsOn(argon_backend_java_api)
      .settings(
      commonJVMSettings,
      Compile / generateNobleIdlJavaAdapters := true,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.vm",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonBrowserSettings,

        Compile / generateNobleIdlScalaJs := true,
        Compile / generateNobleIdlJsAdapters := true,
      )
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonNodeSettings,

        Compile / generateNobleIdlScalaJs := true,
        Compile / generateNobleIdlJsAdapters := true,
      )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-vm",

    Compile / nobleIdlSourceDirectories += baseDirectory.value / "../../backend/vm/",
  )

lazy val argon_vmJVM = argon_vm.jvm
lazy val argon_vmJS = argon_vm.js
lazy val argon_vmNode = argon_vm.node


lazy val argon_backend = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Full).in(file("argon-backend"))
  .enablePlugins(NobleIDLPlugin)
  .dependsOn(util, argon_vm)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.backend",
      ),

      Compile / generateNobleIdlJavaAdapters := true,

      Compile / managedResourceDirectories += resourceManaged.value / "js-backend",
      Compile / resourceGenerators += Def.task {
        val s = streams.value
        val log = s.log

        val resDir = resourceManaged.value / "js-backend"

        def buildJS(packageDir: File): Unit = {
          val installExitCode = Process(Seq("npm", "install"), Some(packageDir)) ! log
          if(installExitCode != 0) {
            throw new Exception("npm install failed with exit code " + installExitCode)
          }

          val distExitCode = Process(Seq("npm", "run", "build"), Some(packageDir)) ! log
          if(distExitCode != 0) {
            throw new Exception("npm run build failed with exit code " + distExitCode)
          }
        }

        def distJS(destFile: File, packageDir: File, distFileName: String): Unit = {
          val installExitCode = Process(Seq("npm", "install"), Some(packageDir)) ! log
          if(installExitCode != 0) {
            throw new Exception("npm install failed with exit code " + installExitCode)
          }

          val distExitCode = Process(Seq("npm", "run", "dist"), Some(packageDir)) ! log
          if(distExitCode != 0) {
            throw new Exception("npm run dist failed with exit code " + distExitCode)
          }

          IO.copyFile(packageDir / "dist" / distFileName, destFile)
        }

        val backendDestFile = resDir / "dev/argon/backend/backends/js/js-backend.js"
        val jsBackendDir = file("backend/backends/js")

        val polyfillDestFile = resDir / "dev/argon/backend/jsApi/polyfill.js"
        val polyfillPackageDir = file("backend/util/graaljs-polyfills")

        val importResDestFile = resDir / "dev/argon/backend/jsApi/import-resolver.js"
        val importResPackageDir = file("backend/util/graaljs-import-resolver")

        val f = FileFunction.cached(s.cacheDirectory / "js-backend") { (in: Set[File]) =>
          log.info("Building JS Backend Distribution")

          buildJS(file("backend/api/js"))
          distJS(backendDestFile, jsBackendDir, "dist-graal.js")
          distJS(polyfillDestFile, polyfillPackageDir, "dist.js")
          buildJS(file("backend/util/js-module-resolution"))
          distJS(importResDestFile, importResPackageDir, "dist.js")

          Set(backendDestFile, polyfillDestFile, importResDestFile)
        }

        val inputFiles = (
          (file("backend/api/src") ** "*.ts") +++
            (
              (jsBackendDir / "src" ** "*.ts")
                --- (jsBackendDir / "src/executor/argon-runtime.ts")
            ) +++
            (polyfillPackageDir / "src" ** "*.js") +++
            (file("backend/util/js-module-resolution/src") ** "*.ts") +++
            (importResPackageDir / "src" ** "*.ts")
        ).get().toSet

        f(inputFiles).toSeq
      }.tag(JSBackendTag).taskValue,
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonBrowserSettings,

        Compile / generateNobleIdlScalaJs := true,
        Compile / generateNobleIdlJsAdapters := true,
      )
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonNodeSettings,

        Compile / generateNobleIdlScalaJs := true,
        Compile / generateNobleIdlJsAdapters := true,
      )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-backend",

    Compile / nobleIdlSourceDirectories += baseDirectory.value / "../../backend/api/nobleidl",
  )

lazy val argon_backendJVM = argon_backend.jvm
lazy val argon_backendJS = argon_backend.js
lazy val argon_backendNode = argon_backend.node


lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-build"))
  .dependsOn(util, argon_compiler, argon_io, parser, argon_vm, argon_source, argon_tube)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,

      Compile / packageBin / packageOptions += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "dev.argon.build",
      ),
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-build",
  )

lazy val argon_buildJVM = argon_build.jvm
lazy val argon_buildJS = argon_build.js
lazy val argon_buildNode = argon_build.node

lazy val compiler_driver_api = project.in(file("argon-compiler-driver-api"))
  .settings(
    commonSettings,
    compilerOptions,

    compileOrder := CompileOrder.JavaThenScala,

    semanticdbEnabled := false,
    autoScalaLibrary := false,
    crossPaths := false,

    name := "argon-compiler-driver-api",
  )

lazy val compiler_driver = crossProject(JVMPlatform, NodePlatform).crossType(CrossType.Full).in(file("argon-compiler-driver"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(util, argon_build, argon_backend)
  .jvmConfigure(
    _.dependsOn(compiler_driver_api)
      .settings(commonJVMSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    libraryDependencies += "com.monovore" %%% "decline" % "2.5.0",

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "dev.argon.driver",
    buildInfoOptions += BuildInfoOption.PackagePrivate,

    name := "argon-compiler-driver",
  )

lazy val compiler_driverJVM = compiler_driver.jvm
lazy val compiler_driverNode = compiler_driver.node

lazy val compiler_launcher = project.in(file("argon-compiler-launcher"))
  .dependsOn(argon_backend_java_api, compiler_driver_api, compiler_driverJVM)
  .settings(
    commonSettingsNoLibs,
    commonJVMSettingsNoLibs,
    compilerOptions,

    compileOrder := CompileOrder.JavaThenScala,

    semanticdbEnabled := false,
    autoScalaLibrary := false,
    crossPaths := false,

    Compile / fork := true,

    name := "argon-compiler-launcher",
  )

lazy val test_runner = project.in(file("argon-test-runner"))
  .settings(
    commonSettingsNoLibs,
    commonJVMSettingsNoLibs,
    compilerOptions,

    compileOrder := CompileOrder.JavaThenScala,

    semanticdbEnabled := false,
    autoScalaLibrary := false,
    crossPaths := false,

    Compile / fork := true,
    Compile / run := (Compile / run).dependsOn(dist),

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "33.5.0-jre",
      "commons-io" % "commons-io" % "2.20.0",
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-xml" % "2.20.0",
      "org.jcommander" % "jcommander" % "2.0",
    ),

    name := "argon-test-runner",
  )
