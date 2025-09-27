import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits.*
import org.scalajs.linker.interface.ESVersion
import complete.DefaultParsers.*
import java.util.Locale

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}

ThisBuild / resolvers += Resolver.mavenLocal
Global / semanticdbEnabled := true

val graalVersion = "25.0.0"
val zioVersion = "2.1.21"

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.7.3",
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

def jsBackendToml(subPath: String): String =
  s"""
     |[backend]
     |api-version = "0.1.0"
     |name = "js"
     |
     |[[loaders]]
     |api = "js"
     |import-path = "$subPath"
     |export-name = "backendFactory"
     |
     |[options.codegen.externs]
     |type = "binary-resource"
     |description = "JS files that export functions used as externs"
     |occurrence = "many"
     |
     |[options.output.modules]
     |type = "directory-resource"
     |description = "Output directory for generated modules"
     |
     |""".stripMargin

distJVM := {
  val distDir = file("dist/argon-jvm")

  val compilerJarDir = distDir / "compiler"
  IO.createDirectory(compilerJarDir)

  val files = (cliJVM / Compile / fullClasspathAsJars)
    .value
    .map { _.data }

  compilerJarDir.listFiles
    .filter { file =>
      file.isFile &&
        file.getName.toUpperCase(Locale.US).endsWith(".jar") &&
        !files.exists { outFile => outFile.getName == file.getName }
    }
    .foreach(IO.delete)



  for (file <- files) {
    IO.copyFile(file, compilerJarDir / file.getName)
  }

  IO.delete(distDir / "backends")
  val _ = distBackendJS.value
  IO.createDirectory(distDir / "backends/js")
  IO.copyFile(file("dist/backends/js/dist/dist-graal.js"), distDir / "backends/js/js-backend.js")
  IO.write(distDir / "backends/js/backend.toml", jsBackendToml("./js-backend.js"))
}

distNode := {
}

distBackends := {
  val _: Unit = distBackendJS.value
}

distBackendJS := {
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
  def npmInstallNoDev(dir: File): Unit = {
    log.info("Installing npm dependencies (omit dev) in " + dir)
    val exitCode = Process(Seq("npm", "install", "--omit=dev"), Some(dir)) ! procLog
    if(exitCode != 0) {
      throw new Exception("npm install --omit=dev failed with exit code " + exitCode)
    }
  }
  def npmRun(dir: File, command: String): Unit = {
    log.info("Running npm run build in " + dir)
    val exitCode = Process(Seq("npm", "run", command), Some(dir)) ! procLog
    if(exitCode != 0) {
      throw new Exception(s"npm run $command failed with exit code " + exitCode)
    }
  }

  val jsApiDir = file("backend/api/js")
  val jsBackendDir = file("backend/backends/js")

  npmInstall(jsApiDir)
  npmRun(jsApiDir, "build")
  npmInstall(jsBackendDir)
  npmRun(jsBackendDir, "dist")
  npmInstallNoDev(jsBackendDir)


  val distBackendDir = file("dist/backends/js")
  IO.delete(distBackendDir)

  log.info("Copying JS backend to " + distBackendDir)
  IO.copyDirectory(jsBackendDir, distBackendDir)

  log.info("Re-installing packages")
  npmInstall(jsBackendDir)

  IO.delete(distBackendDir / "src")
  IO.delete(distBackendDir / "out")
}




lazy val commonSettings = commonSettingsNoLibs ++ Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "dev.argon" %%% "argon-async-util" % "2.1.0",
    "dev.argon.esexpr" %%% "esexpr-scala-runtime" % "0.3.2",
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

  Compile / run / baseDirectory := file(".").getAbsoluteFile,
  Test / baseDirectory := file(".").getAbsoluteFile,

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
    _.settings(commonJVMSettings)
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


lazy val grammar = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-grammar"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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

    name := "argon-grammar",
  )

lazy val grammarJVM = grammar.jvm
lazy val grammarJS = grammar.js
lazy val grammarNode = grammar.node


lazy val ast = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-ast"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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

lazy val esexpr_parser = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("esexpr-parser"))
  .dependsOn(grammar)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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

    name := "esexpr-parser",
  )

lazy val esexpr_parserJVM = esexpr_parser.jvm
lazy val esexpr_parserJS = esexpr_parser.js
lazy val esexpr_parserNode = esexpr_parser.node


lazy val parser = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-parser"))
  .dependsOn(ast, grammar)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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
    _.settings(commonJVMSettings)
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
  .dependsOn(util, esexpr_parser)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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


// lazy val options = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-options"))
//   .dependsOn(util, argon_io)
//   .jvmConfigure(
//     _.settings(commonJVMSettings)
//   )
//   .jsConfigure(
//     _.enablePlugins(NpmUtil)
//       .settings(commonBrowserSettings)
//   )
//   .nodeConfigure(
//     _.enablePlugins(NpmUtil)
//       .settings(commonNodeSettings)
//   )
//   .settings(
//     commonSettings,
//     compilerOptions,

//     name := "argon-options",
//   )

// lazy val optionsJVM = options.jvm
// lazy val optionsJS = options.js
// lazy val optionsNode = options.node


lazy val argon_expr = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-expr"))
  .dependsOn(argon_prover, util, ast)
  .jvmConfigure(
    _.settings(commonJVMSettings)
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
    _.settings(commonJVMSettings)
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
    _.settings(commonJVMSettings)
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

      Compile / generateNobleIdlJavaAdapters := true,

      Compile / managedResourceDirectories += resourceManaged.value / "js-backend",
      Compile / resourceGenerators += Def.task {
        val s = streams.value
        val log = s.log

        val resDir = resourceManaged.value / "js-backend"

        def setupJS(destFile: File, packageDir: File, distFileName: String): Unit = {
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

        val polyfillDestFile = resDir / "dev/argon/backend/polyfill.js"
        val polyfillPackageDir = file("backend/util/graaljs-polyfills")

        val f = FileFunction.cached(s.cacheDirectory / "js-backend") { (in: Set[File]) =>
          log.info("Building JS Backend Distribution")

          setupJS(backendDestFile, jsBackendDir, "dist-graal.js")
          setupJS(polyfillDestFile, polyfillPackageDir, "dist.js")

          Set(backendDestFile, polyfillDestFile)
        }

        val inputFiles = (
          (
            (jsBackendDir / "src" ** "*.ts")
              --- (jsBackendDir / "src/executor/argon-runtime.ts")

          ) +++
            (polyfillPackageDir / "src" ** "*.js")
        ).get().toSet

        f(inputFiles).toSeq
      }.taskValue,
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
    _.settings(commonJVMSettings)
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


lazy val argon_platform = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-platform"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
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

    name := "argon-platform",
  )

lazy val argon_platformJVM = argon_platform.jvm
lazy val argon_platformJS = argon_platform.js
lazy val argon_platformNode = argon_platform.node


lazy val cli = crossProject(JVMPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-cli"))
  .dependsOn(util, argon_platform, argon_build, argon_backend)
  .jvmConfigure(
    _.settings(commonJVMSettings)
      .settings(
        Compile / fork := true,
      )
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
      .settings(
        scalaJSUseMainModuleInitializer := true,
      )
  )
  .settings(
    commonSettings,
    compilerOptions,

    libraryDependencies += "com.github.scopt" %%% "scopt" % "4.1.0",

    name := "argon-cli",
  )

lazy val cliJVM = cli.jvm
lazy val cliNode = cli.node


lazy val compiler_tests = crossProject(JVMPlatform, NodePlatform).crossType(CrossType.Full).in(file("compiler-tests"))
  .dependsOn(util, argon_platform, argon_build, argon_backend)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,


    name := "compiler-tests",
  )

lazy val compiler_testsJVM = compiler_tests.jvm
lazy val compiler_testsNode = compiler_tests.node


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
  .dependsOn(util, argon_platform, argon_build, argon_backend)
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

    libraryDependencies += "com.monovore" %% "decline" % "2.5.0",

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "dev.argon.driver",
    buildInfoOptions += BuildInfoOption.PackagePrivate,

    name := "argon-compiler-driver",
  )

lazy val compiler_driverJVM = compiler_driver.jvm
lazy val compiler_driverNode = compiler_driver.node

lazy val compiler_launcher = project.in(file("argon-compiler-launcher"))
  .dependsOn(argon_backend_java_api, compiler_driver_api)
  .settings(
    commonSettings,
    commonJVMSettings,
    compilerOptions,

    Compile / fork := true,

    name := "argon-compiler-launcher",
  )
