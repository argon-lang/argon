import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.internal.util.ManagedLogger
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

val graalVersion = "21.1.0"
val zioVersion = "2.0.0-M6-2"

ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.20"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

lazy val nodeConfig =
  NodeJSEnv.Config()
    .withEnv(envValues)
    .withArgs(List("--no-warnings", "--experimental-vm-modules"))

val esParseDeps = Seq(
  "acorn" -> "^8.2.4",
)

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.1.0",
)

lazy val commonSettingsAnnotations = Seq(
  libraryDependencies += "org.jetbrains" % "annotations" % "23.0.0",
)

lazy val commonSettings = commonSettingsNoLibs ++ commonSettingsAnnotations ++ Seq(

  resolvers += Resolver.sonatypeRepo("releases"),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-xml" % "2.0.1",

    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
  ),

)

lazy val sharedJVMNodeSettings = Seq(
  Compile / unmanagedSourceDirectories += baseDirectory.value / "../shared-jvm-node/src/main/scala",
  Test / unmanagedSourceDirectories += baseDirectory.value / "../shared-jvm-node/src/test/scala",
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
  ),

  npmDependencies ++= Seq(
    "jszip" -> "^3.7.1",
    "@xmldom/xmldom" -> "^0.7.5",
  ) ++ esParseDeps,
  
  scalaJSLinkerConfig ~= {
    _
      .withModuleKind(ModuleKind.CommonJSModule)
      .withBatchMode(true)
  },

  Compile / unmanagedSourceDirectories += baseDirectory.value / "../shared-js-node/src/main/scala",
  Test / unmanagedSourceDirectories += baseDirectory.value / "../shared-js-node/src/test/scala",

)

lazy val commonJVMSettings = sharedJVMNodeSettings ++ Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-text" % "1.9",
    "org.apache.commons" % "commons-compress" % "1.21",
    "commons-io" % "commons-io" % "2.11.0",
  ),

  Test / fork := true,
  Test / envVars ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val commonNodeSettings = sharedJSNodeSettings ++ sharedJVMNodeSettings ++ Seq(

  npmDependencies ++= Seq(
    "memory-streams" -> "^0.1.3",
    "node-stream-zip" -> "^1.15.0",
  ),

  jsEnv := new NodeJSEnv(nodeConfig),
)

lazy val compilerOptions = Seq(

  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "--release", "11",
    "-Werror",
    "-Xlint:all,-serial,-try",
  ),

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-release", "11",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:strictEquality",
    "-Ycheck-all-patmat",
    "-Xmax-inlines", "128",
    "-Wconf:id=E029:e"
  ),

)



lazy val grammar = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-grammar"))
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

lazy val parser = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-parser"))
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

lazy val parser_data = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-parser-data"))
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

lazy val options = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-options"))
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

    name := "argon-options",
  )

lazy val optionsJVM = options.jvm
lazy val optionsJS = options.js
lazy val optionsNode = options.node


lazy val argon_prover = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-prover"))
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

lazy val argon_expr = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-expr"))
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

lazy val argon_compiler_core = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-compiler-core"))
  .dependsOn(parser_data, util, argon_expr)
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

lazy val argon_packageJVM = argon_compiler_core.jvm
lazy val argon_packageJS = argon_compiler_core.js
lazy val argon_packageNode = argon_compiler_core.node


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
