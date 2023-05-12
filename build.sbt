import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

val graalVersion = "22.3.2"
val zioVersion = "2.0.13"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.2.2",
)

lazy val commonSettings = commonSettingsNoLibs ++ Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %% "zio-direct" % "1.0.0-RC7", // Compile time only dependency

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "dev.zio" %%% "zio-json" % "0.5.0",
    "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.0",

    "org.scala-lang.modules" %%% "scala-xml" % "2.1.0",
    "org.gnieh" %%% "fs2-data-xml-scala" % "1.7.1",
    "dev.zio" %%% "zio-interop-cats" % "23.0.0.5",

    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  ),

)

lazy val npmDeps = Seq(
  "jszip" -> "^3.10.1",
  "acorn" -> "^8.8.2",
  "astring" -> "1.8.4",
)

lazy val npmDevDeps = Seq(
  "typescript" -> "^4.8.4",
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
  ),

  npmDependencies ++= npmDeps,
  npmDevDependencies ++= npmDevDeps,
  
  scalaJSLinkerConfig ~= {
    _
      .withModuleKind(ModuleKind.ESModule)
      .withBatchMode(true)
  },

  externalNpm := {
    (Compile / npmInstall).value
    crossTarget.value
  },

  stIncludeDev := true,
  stUseScalaJsDom := false,

)

lazy val commonJVMSettings = Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-compress" % "1.23.0",
    "commons-io" % "commons-io" % "2.11.0",
    "dev.zio" %% "zio-logging" % "2.1.12",
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

  npmDevDependencies ++= Seq(
    "@types/node" -> "18.8.1",
  )
)

lazy val javaCompilerOptions = Seq(
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "--release", "17",
//    "-Werror",
    "-Xlint:all,-serial,-try,-processing",
  ),
)

lazy val compilerOptions = javaCompilerOptions ++ Seq(


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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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


lazy val argon_prover_prolog = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-prover-prolog"))
  .dependsOn(util, argon_prover)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-prover-prolog",
  )

lazy val argon_prover_prologJVM = argon_prover_prolog.jvm
lazy val argon_prover_prologJS = argon_prover_prolog.js
lazy val argon_prover_prologNode = argon_prover_prolog.node


lazy val argon_prover_smt = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-prover-smt"))
  .dependsOn(util, argon_prover)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-prover-smt",
  )

lazy val argon_prover_smtJVM = argon_prover_smt.jvm
lazy val argon_prover_smtJS = argon_prover_smt.js
lazy val argon_prover_smtNode = argon_prover_smt.node


lazy val argon_expr = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-expr"))
  .dependsOn(argon_prover, argon_prover_prolog, argon_prover_smt, util)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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


lazy val argon_util_protobuf = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-util-protobuf"))
  .dependsOn(util)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,

    name := "argon-util-protobuf",
  )

lazy val argon_util_protobufJVM = argon_util_protobuf.jvm
lazy val argon_util_protobufJS = argon_util_protobuf.js
lazy val argon_util_protobufNode = argon_util_protobuf.node


lazy val argon_tube = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-tube"))
  .dependsOn(util, argon_util_protobuf)
  .jvmConfigure(
    _.dependsOn(argon_plugin_java_api).settings(commonJVMSettings)
      .settings(
        Compile / PB.targets := Seq(
          scalapb.gen(javaConversions=true) -> (Compile / sourceManaged).value / "scalapb"
        ),
      )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
      .settings(
        Compile / PB.targets := Seq(
          scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
        ),
      )
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
      .settings(
        Compile / PB.targets := Seq(
          scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
        ),
      )
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

    Compile / PB.protoSources += (Compile / baseDirectory).value.getParentFile / "src/main/protobuf",
    Compile / PB.protoSources += (Compile / baseDirectory).value.getParentFile.getParentFile / "plugin/api/protobuf",

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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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


lazy val argon_test_util = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-test-util"))
  .dependsOn(util, argon_io)
  .jvmConfigure(
    _.settings(
        commonJVMSettings,
      )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,
    
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion,

    name := "argon-test-util",
  )

lazy val argon_test_utilJVM = argon_test_util.jvm
lazy val argon_test_utilJS = argon_test_util.js
lazy val argon_test_utilNode = argon_test_util.node


lazy val argon_plugin_test_util = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugin-test-util"))
  .dependsOn(argon_test_util, argon_plugin, argon_plugins_source)
  .jvmConfigure(
    _.settings(
      commonJVMSettings,
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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


lazy val argon_plugins_tube = crossProject(JVMPlatform, JSPlatform, NodePlatform).crossType(CrossType.Pure).in(file("argon-plugins-tube"))
  .dependsOn(argon_compiler_core, argon_tube, argon_plugin)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-plugins-tube",
  )

lazy val argon_plugins_tubeJVM = argon_plugins_tube.jvm
lazy val argon_plugins_tubeJS = argon_plugins_tube.js
lazy val argon_plugins_tubeNode = argon_plugins_tube.node


lazy val argon_plugin_java_api = project.in(file("plugin/api/java"))
  .settings(
    commonSettingsNoLibs,
    compilerOptions,

    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % "3.23.0",
      "com.google.protobuf" % "protobuf-java" % "3.23.0" % "protobuf",
      "org.jetbrains" % "annotations" % "24.0.1",
    ),

    Compile / PB.targets := Seq(
      PB.gens.java -> (Compile / sourceManaged).value / "protobuf",
    ),

    Compile / PB.protoSources += (Compile / baseDirectory).value.getParentFile / "protobuf",

    autoScalaLibrary := false,
    fork := true,

    name := "argon-plugin-java-api",
  )


lazy val argon_plugin_java_sourcegen = project.in(file("plugin/util/java/java-sourcegen"))
  .settings(
    commonSettingsNoLibs,
    compilerOptions,

    libraryDependencies += "org.apache.commons" % "commons-text" % "1.10.0",

    autoScalaLibrary := false,
    fork := true,

    name := "argon-plugin-java-sourcegen",
  )

lazy val argon_plugin_js_api = crossProject(JSPlatform, NodePlatform).in(file("argon-plugin-js-api"))
  .dependsOn(argon_plugin)
  .jsConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-plugin-js-api",
  )

lazy val argon_plugin_js_apiJS = argon_plugin_js_api.js
lazy val argon_plugin_js_apiNode = argon_plugin_js_api.node

lazy val argon_plugin_loader = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugin-loader"))
  .dependsOn(argon_plugin)
  .jvmConfigure(
    _.dependsOn(argon_plugin_java_api)
      .settings(commonJVMSettings)
      .settings(
        libraryDependencies ++= Seq(
          "org.graalvm.sdk" % "graal-sdk" % graalVersion,
          "org.graalvm.js" % "js" % graalVersion,
          "org.graalvm.js" % "js-scriptengine" % graalVersion,
          "org.graalvm.tools" % "profiler" % graalVersion,
          "org.graalvm.tools" % "chromeinspector" % graalVersion,
        ),
      )
  )
  .jsConfigure(
    _.dependsOn(argon_plugin_js_apiJS)
      .enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
      .settings(
        npmDependencies += "@argon-lang/plugin-api" -> "file:../../../../plugin/api/js",
      )
  )
  .nodeConfigure(
    _.dependsOn(argon_plugin_js_apiNode)
      .enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonNodeSettings)
      .settings(
        npmDependencies += "@argon-lang/plugin-api" -> "file:../../../../plugin/api/js",
      )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-plugin-loader",
  )

lazy val argon_plugin_loaderJVM = argon_plugin_loader.jvm
lazy val argon_plugin_loaderJS = argon_plugin_loader.js
lazy val argon_plugin_loaderNode = argon_plugin_loader.node


lazy val argon_plugins_tube_java = project.in(file("plugin/tube/java"))
  .dependsOn(argon_plugin_java_api % "provided", argon_plugin_java_sourcegen % "provided")
  .settings(
    commonSettingsNoLibs,
    compilerOptions,

    javacOptions ++= Seq(
      "-processor", "dev.argon.plugin.util.javasourcegen.OptionsProcessor",
    ),

    autoScalaLibrary := false,
    fork := true,

    name := "argon-plugin-tube-java",
  )


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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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
    _.enablePlugins(NpmUtil, ScalablyTypedConverterExternalNpmPlugin)
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



