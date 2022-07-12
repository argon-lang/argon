import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

import scala.sys.process.Process

val graalVersion = "22.1.0.1"
val zioVersion = "2.0.0"

ThisBuild / semanticdbEnabled := true
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.20"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

lazy val nodeConfig =
  NodeJSEnv.Config()
    .withEnv(envValues)
    .withArgs(List("--no-warnings", "--experimental-vm-modules"))

lazy val commonSettingsNoLibs = Seq(
  scalaVersion := "3.1.3",
)

lazy val commonSettings = commonSettingsNoLibs ++ Seq(

  resolvers += Resolver.sonatypeRepo("releases"),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.1.4",
  ),

)

lazy val sharedJVMNodeSettings = Seq(
  Compile / unmanagedSourceDirectories += baseDirectory.value / "../shared-jvm-node/src/main/scala",
  Test / unmanagedSourceDirectories += baseDirectory.value / "../shared-jvm-node/src/test/scala",
)

lazy val npmDeps = Seq(
  "jszip" -> "^3.10.0",
  "@xmldom/xmldom" -> "^0.8.2",
  "acorn" -> "^8.7.1",
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

  Compile / unmanagedSourceDirectories += baseDirectory.value / "../shared-js-node/src/main/scala",
  Test / unmanagedSourceDirectories += baseDirectory.value / "../shared-js-node/src/test/scala",

)

lazy val commonJVMSettings = sharedJVMNodeSettings ++ Seq(

  libraryDependencies ++= Seq(
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
    "--release", "17",
    "-Werror",
    "-Xlint:all,-serial,-try",
  ),

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-release", "17",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:strictEquality",
    "-Ycheck-all-patmat",
    "-Yretain-trees",
    "-Xmax-inlines", "128",
    "-Wconf:id=E029:e,cat=unchecked:e",
  ),

)



lazy val generateVerilization = taskKey[Seq[File]]("Run verilization compiler to generate source code.")






lazy val verilization_runtimeJVM = ProjectRef(file("tools/verilization/scala"), "scalaRuntimeJVM")
lazy val verilization_runtimeJS = ProjectRef(file("tools/verilization/scala"), "scalaRuntimeJS")

lazy val verRuntime = Map("" -> "dev.argon.verilization.runtime.zio")
lazy val verTube = Map("argon.tube" -> "dev.argon.tube")
lazy val verPlugin = Map("argon.plugin" -> "dev.argon.plugin.rpc")

def runVerilization(libraries: Map[String, String], packageMap: Map[String, String], inputFiles: Set[File]): Def.Initialize[Task[Seq[File]]] = Def.task {
  val log = streams.value.log
  val cached = FileFunction.cached(streams.value.cacheDirectory / "verilization") { (inputFiles: Set[File]) =>
    val outputDir = sourceManaged.value / "verilization"
    log.info(s"Generating verilization definitions to ${outputDir}")

    val libraryFiles = (file("tools/verilization/verilization/runtime") ** "*.verilization").get()


    val command =
      Seq(
        "cargo", "run", "--quiet", "--manifest-path", "tools/verilization/rust/compiler-cli/Cargo.toml", "--",
        "generate", "scala",
      ) ++
        (libraryFiles ++ inputFiles.toSeq).flatMap { inputFile => Seq("-i", inputFile.toString) } ++
        Seq("-o:out_dir", outputDir.toString) ++
        libraries.toSeq.flatMap { case (verPkg, targetPkg) => Seq(s"-o:lib:$verPkg", targetPkg) } ++
        packageMap.toSeq.flatMap { case (verPkg, targetPkg) => Seq(s"-o:pkg:$verPkg", targetPkg) }

    IO.delete(outputDir)

    val exitCode = Process(command) ! log

    if(exitCode != 0) {
      throw new Exception("Could not generate verilization definitions")
    }

    (outputDir ** "*.scala").get().toSet
  }

  cached(inputFiles).toSeq
}



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


lazy val argon_compiler_source = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-compiler-source"))
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

    name := "argon-compiler-source",
  )

lazy val argon_compiler_sourceJVM = argon_compiler_source.jvm
lazy val argon_compiler_sourceJS = argon_compiler_source.js
lazy val argon_compiler_sourceNode = argon_compiler_source.node


lazy val argon_tube = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-tube"))
  .dependsOn(util)
  .jvmConfigure(
    _.dependsOn(verilization_runtimeJVM)
      .settings(commonJVMSettings)
  )
  .jsConfigure(
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-tube",

    Compile / sourceGenerators += generateVerilization,
    generateVerilization := runVerilization(
      libraries = verRuntime,
      packageMap = verTube,
      inputFiles = Set(file("argon-tube/tube.verilization")),
    ).value,
  )

lazy val argon_tubeJVM = argon_tube.jvm
lazy val argon_tubeJS = argon_tube.js
lazy val argon_tubeNode = argon_tube.node


lazy val argon_plugin = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugin"))
  .dependsOn(util, argon_tube, argon_compiler_core)
  .jvmConfigure(
    _.dependsOn(verilization_runtimeJVM)
      .settings(
        commonJVMSettings,
      )
  )
  .jsConfigure(
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
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


lazy val argon_plugin_js = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-plugins-js"))
  .dependsOn(util, argon_tube, argon_plugin)
  .jvmConfigure(
    _.dependsOn(verilization_runtimeJVM)
      .settings(
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
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.dependsOn(verilization_runtimeJS)
      .enablePlugins(NpmUtil)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-plugins-js",
  )

lazy val argon_plugin_jsJVM = argon_plugin_js.jvm
lazy val argon_plugin_jsJS = argon_plugin_js.js
lazy val argon_plugin_jsNode = argon_plugin_js.node


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


lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-build"))
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

    name := "argon-build",
  )

lazy val argon_buildJVM = argon_build.jvm
lazy val argon_buildJS = argon_build.js
lazy val argon_buildNode = argon_build.node




