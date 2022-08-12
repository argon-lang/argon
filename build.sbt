import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

import scala.sys.process.Process

val graalVersion = "22.2.0"
val zioVersion = "2.0.0"

ThisBuild / semanticdbEnabled := true
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.20"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.1.3",
)

lazy val commonSettings = commonSettingsNoLibs ++ Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "dev.zio" %%% "zio-json" % "0.3.0-RC10",
    "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.1.5",


  ),

)

lazy val npmDeps = Seq(
  "jszip" -> "^3.10.1",
  "@xmldom/xmldom" -> "^0.8.2",
  "acorn" -> "^8.8.0",
  "astring" -> "1.8.3",
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.4.0",
  ),

  npmDependencies ++= npmDeps,
  
  scalaJSLinkerConfig ~= {
    _
      .withModuleKind(ModuleKind.CommonJSModule)
      .withBatchMode(true)
  },

)

lazy val commonJVMSettings = Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-compress" % "1.21",
    "commons-io" % "commons-io" % "2.11.0",
    "dev.zio" %% "zio-logging" % "2.0.1",
  ),

  Test / fork := true,
  Test / envVars ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val commonNodeSettings = sharedJSNodeSettings ++ Seq(

  jsEnv := new NodeJSEnv(
    NodeJSEnv.Config()
      .withEnv(envValues)
      .withArgs(List("--no-warnings", "--experimental-vm-modules"))
  ),
)

lazy val compilerOptions = Seq(

  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "--release", "17",
    "-Werror",
    "-Xlint:all,-serial,-try",
  ),

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-release", "17",
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


lazy val parser = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-parser"))
  .dependsOn(parser_data, grammar)
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


lazy val parser_data = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-parser-data"))
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

    name := "argon-parser-data",
  )

lazy val parser_dataJVM = parser_data.jvm
lazy val parser_dataJS = parser_data.js
lazy val parser_dataNode = parser_data.node


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


lazy val options = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-options"))
  .dependsOn(util, argon_io)
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


lazy val argon_expr = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-expr"))
  .dependsOn(argon_prover, util)
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


lazy val argon_compiler_core = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-compiler-core"))
  .dependsOn(parser_data, util, argon_expr, options, argon_io)
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


lazy val argon_tube = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-tube"))
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

    scalacOptions := scalacOptions.value.filterNot(Seq(
      "-source",
      "future",
      "-language:strictEquality",
      "-Yexplicit-nulls",
    ).contains),

    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    Compile / PB.protoSources += (Compile / baseDirectory).value.getParentFile / "src/main/protobuf",

    name := "argon-tube",
  )

lazy val argon_tubeJVM = argon_tube.jvm
lazy val argon_tubeJS = argon_tube.js
lazy val argon_tubeNode = argon_tube.node


lazy val argon_plugin = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugin"))
  .dependsOn(util, argon_tube, argon_compiler_core)
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

    name := "argon-plugin",
  )

lazy val argon_pluginJVM = argon_plugin.jvm
lazy val argon_pluginJS = argon_plugin.js
lazy val argon_pluginNode = argon_plugin.node


lazy val argon_plugin_test_util = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugin-test-util"))
  .dependsOn(argon_plugin, argon_plugins_source)
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
    
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion,

    name := "argon-plugin-test-util",
  )

lazy val argon_plugin_test_utilJVM = argon_plugin_test_util.jvm
lazy val argon_plugin_test_utilJS = argon_plugin_test_util.js
lazy val argon_plugin_test_utilNode = argon_plugin_test_util.node


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


lazy val argon_plugins_js = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugins-js"))
  .dependsOn(util, argon_tube, argon_plugin, argon_plugin_test_util % "test->compile")
  .jvmConfigure(
    _.settings(
        commonJVMSettings,

        libraryDependencies ++= Seq(
          "org.graalvm.sdk" % "graal-sdk" % graalVersion,
          "org.graalvm.js" % "js" % graalVersion,
          "org.graalvm.js" % "js-scriptengine" % graalVersion,
          "org.graalvm.tools" % "profiler" % graalVersion,
          "org.graalvm.tools" % "chromeinspector" % graalVersion,
        ),

        Compile / resourceGenerators += Def.task {
          val resourceDir = (Compile / resourceManaged).value
          val targetDir = crossTarget.value

          NpmUtil.npmInstallCommon(
            name = name.value,
            packageLock = baseDirectory.value / "package-lock.json",
            dir = targetDir,
            linkerConfig = org.scalajs.linker.interface.StandardConfig()
              .withModuleKind(ModuleKind.CommonJSModule),

            npmDependencies = npmDeps,
            npmDevDependencies = Seq.empty,
          )


          val dir = resourceDir / "dev" / "argon" / "plugins" / "js"
          IO.createDirectory(dir)

          val astringFile = dir / "astring.mjs"
          IO.copyFile(targetDir / "node_modules" / "astring" / "dist" / "astring.mjs", astringFile)

          val acornFile = dir / "acorn.mjs"
          IO.copyFile(targetDir / "node_modules" / "acorn" / "dist" / "acorn.mjs", acornFile)

          Seq(astringFile, acornFile)
        }.taskValue
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

    name := "argon-plugins-js",
  )

lazy val argon_plugins_jsJVM = argon_plugins_js.jvm
lazy val argon_plugins_jsJS = argon_plugins_js.js
lazy val argon_plugins_jsNode = argon_plugins_js.node


lazy val argon_io = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-io"))
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

    name := "argon-io",
  )

lazy val argon_ioJVM = argon_io.jvm
lazy val argon_ioJS = argon_io.js
lazy val argon_ioNode = argon_io.node


lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-build"))
  .dependsOn(util, options, argon_compiler_core, argon_io, argon_plugin)
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
  .dependsOn(util, options, argon_compiler_core, argon_io, argon_build, argon_plugins_source, argon_plugins_js)
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




