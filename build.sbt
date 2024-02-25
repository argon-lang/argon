import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits.*
import org.apache.commons.lang3.StringUtils
import org.apache.commons.io.FilenameUtils

import java.io.File
import java.nio.charset.StandardCharsets
import scala.sys.process.Process

val graalVersion = "23.1.0"
val zioVersion = "2.0.21"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.3.1",
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
    "org.typelevel" %% "cats-core" % "2.10.0",
    "dev.zio" %%% "zio-interop-cats" % "23.1.0.0",
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
    "org.apache.commons" % "commons-compress" % "1.25.0",
    "dev.zio" %% "zio-logging" % "2.1.16",
    "net.java.dev.jna" % "jna" % "5.14.0",
    "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
  ),

  fork := true,
  Test / envVars ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val nodeEnvConfig =
  NodeJSEnv.Config()
    .withEnv(envValues)
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
    "-Wconf:id=E029:e,id=E165:e,cat=unchecked:e,cat=deprecation:e",
  ),

)

def escapeESExprString(s: String): String =
  s.replace("\\", "\\\\").replace("\"", "\\\"")

def generateESExprTask(config: File => String): Def.Initialize[_root_.sbt.Task[Seq[File]]] =
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


/*
lazy val argon_compiler_core = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-compiler-core"))
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

    name := "argon-compiler-core",
  )

lazy val argon_compiler_coreJVM = argon_compiler_core.jvm
lazy val argon_compiler_coreJS = argon_compiler_core.js
lazy val argon_compiler_coreNode = argon_compiler_core.node

lazy val argonvm = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argonvm"))
  .dependsOn(argon_compiler_core)
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

    name := "argonvm",
  )

lazy val argonvmJVM = argonvm.jvm
lazy val argonvmJS = argonvm.js
lazy val argonvmNode = argonvm.node

lazy val argon_plugin = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugin"))
  .dependsOn(util, argon_compiler_core, esexpr, argonvm)
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
           esx-file: "${escapeESExprString((dir / "../../plugin/api/esx/tube.esx").getAbsolutePath)}"
           out-file: "api_tube.scala"
           package-name: "dev.argon.plugin.tube"
        )
        """
    ),

    name := "argon-plugin",
  )

lazy val argon_pluginJVM = argon_plugin.jvm
lazy val argon_pluginJS = argon_plugin.js
lazy val argon_pluginNode = argon_plugin.node



lazy val argon_plugins_source = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugins-source"))
  .dependsOn(argon_compiler_core, parser, argon_plugin)
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

lazy val argon_plugins_lua = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Full).in(file("argon-plugins-lua"))
  .dependsOn(argon_compiler_core, parser, argon_plugin)
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

    name := "argon-plugins-lua",
  )

lazy val argon_plugins_luaJVM = argon_plugins_lua.jvm
lazy val argon_plugins_luaJS = argon_plugins_lua.js
lazy val argon_plugins_luaNode = argon_plugins_lua.node

lazy val argon_plugins_wasm = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Full).in(file("argon-plugins-wasm"))
  .dependsOn(argon_compiler_core, parser, argon_plugin)
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

    name := "argon-plugins-wasm",
  )

lazy val argon_plugins_wasmJVM = argon_plugins_wasm.jvm
lazy val argon_plugins_wasmJS = argon_plugins_wasm.js
lazy val argon_plugins_wasmNode = argon_plugins_wasm.node

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



lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-build"))
  .dependsOn(util, options, argon_compiler_core, argon_io, argon_plugin, argon_plugin_platform)
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

*/

