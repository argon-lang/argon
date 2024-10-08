import java.io.PrintWriter
import java.io.FileOutputStream
import sbt.io.Using
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits.*
import org.apache.commons.lang3.StringUtils
import org.apache.commons.io.FilenameUtils
import org.apache.commons.text.StringEscapeUtils
import org.scalajs.linker.interface.ESVersion

import java.io.File
import java.nio.charset.StandardCharsets
import scala.sys.process.Process

Global / semanticdbEnabled := true

val graalVersion = "23.1.0"
val zioVersion = "2.0.21"

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.4.0",
)

lazy val commonSettings = commonSettingsNoLibs ++ Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "dev.zio" %%% "zio-json" % "0.6.2",
    "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.4",

    "org.scala-lang.modules" %%% "scala-xml" % "2.2.0",
    "org.gnieh" %%% "fs2-data-xml-scala" % "1.10.0",
    "org.typelevel" %%% "cats-core" % "2.10.0",
    "dev.zio" %%% "zio-interop-cats" % "23.1.0.1",
  ),

)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
  ),

  npmDependencies ++= Seq(
    "jszip" -> "3.10.1",
    "acorn" -> "8.11.3",
    "astring" -> "1.8.6",
  ),
  
  scalaJSLinkerConfig ~= {
    _
      .withModuleKind(ModuleKind.ESModule)
      .withBatchMode(true)
      .withESFeatures(_.withESVersion(ESVersion.ES2021))
  },

  fork := false,

)

lazy val graalDependencies = Seq(
  "org.graalvm.polyglot" % "polyglot" % graalVersion,
  "org.graalvm.polyglot" % "js" % graalVersion,
  "org.graalvm.polyglot" % "wasm" % graalVersion,
)

lazy val annotationDependencies = Seq(
  "org.jetbrains" % "annotations" % "24.1.0",
)

lazy val commonJVMSettings = Seq(

  libraryDependencies ++= annotationDependencies ++ Seq(
    "commons-io" % "commons-io" % "2.15.1",
    "org.apache.commons" % "commons-lang3" % "3.14.0",
    "org.apache.commons" % "commons-text" % "1.11.0",
    "org.apache.commons" % "commons-compress" % "1.26.1",
    "dev.zio" %% "zio-logging" % "2.2.2",
    "net.java.dev.jna" % "jna" % "5.14.0",
    "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
  ),

  fork := true,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val nodeEnvConfig =
  NodeJSEnv.Config()
    .withArgs(List("--no-warnings", "--experimental-vm-modules"))

lazy val commonNodeSettings = sharedJSNodeSettings ++ Seq(

  jsEnv := new NodeJSEnv(nodeEnvConfig),

  npmDevDependencies ++= Seq(
    "@types/node" -> "18.8.1",
  )
)

lazy val javaCompilerOptions = Seq(
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "--release", "21",
//    "-Werror",
    "-Xlint:all,-serial,-try,-processing",
  ),
)

lazy val javaOnlyOptions = javaCompilerOptions ++ commonSettingsNoLibs

lazy val compilerOptions = javaCompilerOptions ++ Seq(


  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-release", "21",
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

)

def escapeESExprString(s: String): String =
  s.replace("\\", "\\\\").replace("\"", "\\\"")

def generateESExprTask(config: File => String): Def.Initialize[sbt.Task[Seq[File]]] =
  Def.task {
    val s = streams.value
    val baseDir = baseDirectory.value
    val managedDir = sourceManaged.value

    val javaArgs = javaOptions.value
    val generatorCP = (esexpr_generator / Compile / fullClasspath).value.map(_.data.toString).mkString(File.pathSeparator)

    val genRunner = (Compile / runner).value

    IO.withTemporaryFile("generator-", ".esx") { configFile =>
      IO.write(configFile, config(baseDir), StandardCharsets.UTF_8)

      val f = FileFunction.cached(s.cacheDirectory / "generate-esx") { (in: Set[File]) =>
        val outDir = managedDir / "generate-esx"
        IO.createDirectory(outDir)

        s.log.info(s"Generating sources from ESExpr schema in ${outDir}")

        IO.createDirectory(outDir)
        val exitCode = Process(Seq("java") ++ javaArgs ++ Seq("-cp", generatorCP, "dev.argon.esexpr.generator.Generator", configFile.getAbsolutePath, outDir.getAbsolutePath)).!(s.log)
        if (exitCode != 0) {
          throw new Exception(s"ESExpr Generator failed with exit code ${exitCode}")
        }

        outDir.allPaths.filter(_.isFile).get().toSet
      }

      val inputFilesJson = Process(Seq("java") ++ javaArgs ++ Seq("-cp", generatorCP, "dev.argon.esexpr.generator.GeneratorInputs", configFile.getAbsolutePath)).!!(s.log)

      f(implicitly[_root_.io.circe.Decoder[Seq[String]]].decodeJson(_root_.io.circe.parser.parse(inputFilesJson).right.get).right.get.map(new File(_)).toSet).toSeq
    }
  }

def generateTestCasesTask(): Def.Initialize[sbt.Task[Seq[File]]] =
  Def.task {
    import java.nio.file.{Path, Files}

    val s = streams.value
    val managedDir = sourceManaged.value

    def generateFSMapFile(inDir: File, outName: String, mapName: String)(in: Set[File]): Set[File] = {
      val inDirPath = inDir.toPath().toAbsolutePath()

      val outDir = managedDir / "generated-test-cases"
      IO.createDirectory(outDir)

      val outFile = outDir / outName

      s.log.info(s"Generating sources from test cases in ${outDir}")

      IO.createDirectory(outDir)
      
      val writer = new PrintWriter(outFile, StandardCharsets.UTF_8)
      try {
        writer.println("package dev.argon.compiler_tests")

        writer.println(s"val $mapName: Map[String, String] = Map(")

        for(file <- in) {
          val path = file.toPath().toAbsolutePath()
          val relPath = inDirPath.relativize(path).toString
          val content = Files.readString(path)

          writer.print("  \"")
          writer.print(StringEscapeUtils.escapeJava(relPath))
          writer.print("\" -> \"")
          writer.print(StringEscapeUtils.escapeJava(content))
          writer.println("\",")
        }

        writer.println(")")
      }
      finally writer.close()

      Set(outFile)
    }

    val f1 = FileFunction.cached(s.cacheDirectory / "generated-test-cases")(generateFSMapFile(file("testcases"), "TestCases.scala", "testCases"))
    val f2 = FileFunction.cached(s.cacheDirectory / "generated-libraries")(generateFSMapFile(file("libraries"), "Libraries.scala", "libraries"))

    f1((file("testcases") ** "*.xml").get().toSet).toSeq ++
      f2((
        file("libraries") ** "*" ---
          file("libraries") ** "bin" ** "*"
      ).filter(_.isFile()).get().toSet).toSeq

      
  }


lazy val esexpr = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("esexpr"))
  .dependsOn(util, grammar)
  .jvmConfigure(_.settings(commonJVMSettings))
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

    name := "esexpr",
  )

lazy val esexprJVM = esexpr.jvm
lazy val esexprJS = esexpr.js
lazy val esexprNode = esexpr.node


lazy val esexpr_generator = project.in(file("esexpr-generator"))
  .dependsOn(esexprJVM)
  .settings(
    commonJVMSettings,
    commonSettings,
    compilerOptions,

    name := "esexpr-generator",
  )

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

    name := "argon-parser",
  )

lazy val parserJVM = parser.jvm
lazy val parserJS = parser.js
lazy val parserNode = parser.node


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

    name := "argon-util",
  )

lazy val utilJVM = util.jvm
lazy val utilJS = util.js
lazy val utilNode = util.node


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
  .dependsOn(util, esexpr)
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


lazy val options = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-options"))
  .dependsOn(util, argon_io, esexpr)
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

    name := "argon-options",
  )

lazy val optionsJVM = options.jvm
lazy val optionsJS = options.js
lazy val optionsNode = options.node


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
  .dependsOn(ast, util, argon_expr, options, argon_io)
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


lazy val argon_plugin = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugin"))
  .dependsOn(util, argon_compiler, esexpr)
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

    Compile / sourceGenerators += generateESExprTask(dir =>
      s"""
        (scala
           esx-file: "${escapeESExprString((dir / "../../plugin/api/esx/vm.esx").getAbsolutePath)}"
           out-file: "api_vm.scala"
           package-name: "dev.argon.plugin.vm"
        )
        """
    ),

    name := "argon-plugin",
  )

lazy val argon_pluginJVM = argon_plugin.jvm
lazy val argon_pluginJS = argon_plugin.js
lazy val argon_pluginNode = argon_plugin.node


lazy val argon_plugins_source = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugins-source"))
  .dependsOn(argon_compiler, parser, argon_plugin)
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

    name := "argon-plugins-source",
  )

lazy val argon_plugins_sourceJVM = argon_plugins_source.jvm
lazy val argon_plugins_sourceJS = argon_plugins_source.js
lazy val argon_plugins_sourceNode = argon_plugins_source.node


lazy val argon_plugin_platform = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugin-platform"))
  .dependsOn(argon_plugin, argon_plugins_source)
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

    name := "argon-plugin-platform",
  )

lazy val argon_plugin_platformJVM = argon_plugin_platform.jvm
lazy val argon_plugin_platformJS = argon_plugin_platform.js
lazy val argon_plugin_platformNode = argon_plugin_platform.node


lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-build"))
  .dependsOn(util, options, argon_compiler, argon_io, argon_plugin, argon_plugin_platform)
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


lazy val compiler_tests = crossProject(JVMPlatform, NodePlatform).crossType(CrossType.Full).in(file("compiler-tests"))
  .dependsOn(util, argon_platform, argon_build)
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

    Compile / sourceGenerators += generateTestCasesTask(),

    name := "compiler-tests",
  )

lazy val compiler_testsJVM = compiler_tests.jvm
lazy val compiler_testsNode = compiler_tests.node


lazy val cli = crossProject(JVMPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-cli"))
  .dependsOn(util, argon_platform, argon_build)
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

