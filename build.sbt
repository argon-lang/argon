import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.internal.util.ManagedLogger
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

val graalVersion = "21.1.0"
val zioVersion = "1.0.8"

ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.18"

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
  scalaVersion := "2.13.5",
  crossScalaVersions := Seq("3.0.0", "2.13.5"),
)

lazy val commonSettingsAnnotations = Seq(
  libraryDependencies += "org.jetbrains" % "annotations" % "20.1.0",
)

lazy val commonSettings = commonSettingsNoLibs ++ commonSettingsAnnotations ++ Seq(

  resolvers += Resolver.sonatypeRepo("releases"),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-xml" % "2.0.0",

    "org.typelevel" %%% "cats-core" % "2.6.1",
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,


    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq(
      "dev.zio" %%% "zio-interop-cats" % "3.1.1.0",
    )
    case _ => Seq(
      "dev.zio" %%% "zio-interop-cats" % "3.0.2.0",
      "org.typelevel" %%% "kittens" % "2.3.1",
      "com.chuusai" %%% "shapeless" % "2.3.6",
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),
    )
  }),

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
    "jszip" -> "^3.6.0",
    "xmldom" -> "^0.6.0",
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
    "org.apache.commons" % "commons-compress" % "1.20",
    "commons-io" % "commons-io" % "2.8.0",
  ),

  Test / fork := true,
  Test / envVars ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val commonNodeSettings = sharedJSNodeSettings ++ sharedJVMNodeSettings ++ Seq(

  npmDependencies ++= Seq(
    "memory-streams" -> "^0.1.3",
    "node-stream-zip" -> "^1.13.4",
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
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq(
      "-Ykind-projector",
      "-source:3.0-migration",
    )
    case _ => Seq (
      "-Xsource:3",
      "-Yrangepos",
      "-Wunused",
      "-Wconf:cat=lint:error," +
        "cat=deprecation:error," +
        "cat=feature:error," +
        "cat=optimizer:error," +
        "msg=match may not be exhaustive\\.:error," +
        "cat=unchecked&msg=The outer reference in this type test cannot be checked at run time\\.:silent," +
        "cat=unchecked:error," +
        "cat=unused-imports:warning," +
        "cat=java-source:error," +
        "cat=w-flag-dead-code:silent," +
        "cat=w-flag:error," +
        "cat=other-match-analysis&msg=unreachable code:silent",
      "-Ypatmat-exhaust-depth", "2000",
    )
  }),

  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  scalafixOnCompile := CrossVersion.partialVersion(scalaVersion.value).forall { case (3, _) => false case _ => true },
)

def argonLibraries = Seq("Argon.Core")

lazy val buildArgonLibs = taskKey[Unit]("Compile Argon libraries")


lazy val cli = crossProject(JVMPlatform, NodePlatform).in(file("argon-cli"))
  .dependsOn(argon_build, argon_platform)
  .jvmConfigure(
    _.settings(commonJVMSettings,

      buildArgonLibs := {
        val r = (Compile / runner).value
        val classpath = (Compile / fullClasspath).value
        val log = streams.value.log

        ArgonLibraryBuild.buildLibraries(log)(argonLibraries) { commandArgs =>
          Run.run(
            "dev.argon.Program",
            classpath.map { _.data },
            commandArgs,
            log
          )(r).get
        }
      },

    )
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonNodeSettings,

        scalaJSUseMainModuleInitializer := true,

        buildArgonLibs := {
          npmInstall.value
          val log = streams.value.log
          val jsFile = (Compile / fastOptJS).value.data

          ArgonLibraryBuild.buildLibraries(log)(argonLibraries) { commandArgs =>
            import scala.sys.process.Process
            Process(
              (nodeConfig.executable +: nodeConfig.args) ++ (jsFile.toString +: commandArgs),
              file("."),
              nodeConfig.env.toSeq: _*
            ).!(log)

          }
        },

    )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-cli",
  )

lazy val cliJVM = cli.jvm
lazy val cliNode = cli.node

lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-build"))
  .dependsOn(argon_build_base, backend_js, backend_generic, argon_platform % "test")
  .jvmConfigure(
    _.settings(commonJVMSettings)
     .settings(
       libraryDependencies ++= Seq(
         "org.graalvm.sdk" % "graal-sdk" % graalVersion,
         "org.graalvm.js" % "js" % graalVersion,
         "org.graalvm.js" % "js-scriptengine" % graalVersion,
         "org.graalvm.tools" % "profiler" % graalVersion,
         "org.graalvm.tools" % "chromeinspector" % graalVersion,
       )
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

lazy val argon_build_base = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-build-base"))
  .dependsOn(parser, armodule)
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

    name := "argon-build-base",
  )

lazy val argon_build_baseJVM = argon_build_base.jvm
lazy val argon_build_baseJS = argon_build_base.js
lazy val argon_build_baseNode = argon_build_base.node


lazy val grammar = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-grammar"))
  .dependsOn(arstream, util)
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
  .dependsOn(parser_data, grammar, argon_compiler_core)
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

lazy val argon_compiler = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-compiler"))
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

    name := "argon-compiler",
  )

lazy val argon_compilerJVM = argon_compiler.jvm
lazy val argon_compilerJS = argon_compiler.js
lazy val argon_compilerNode = argon_compiler.node

lazy val argon_compiler_core = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-compiler-core"))
  .dependsOn(argonio, modulefmt, parser_data, options)
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

lazy val armodule = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-armodule"))
  .dependsOn(argon_compiler)
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

    name := "argon-armodule",
  )

lazy val armoduleJVM = armodule.jvm
lazy val armoduleJS = armodule.js
lazy val armoduleNode = armodule.node

lazy val backend_js = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-backend-js"))
  .dependsOn(argon_compiler)
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

        (js_module_extractor / Compile / npmInstall).value

        val moduleExtractorDir = (js_module_extractor / Compile / crossTarget).value
        val resDir = (Compile / resourceManaged).value / "dev" / "argon" / "js_module_extractor"

        val moduleExtractorFile = (js_module_extractor / Compile / fullOptJS).value.data
        val moduleExtractorResFile = resDir / "js-module-extractor.mjs"

        def extraFiles(baseDir: File): Seq[File] = Seq(
          baseDir / "package.json",
          baseDir / "node_modules" / "acorn" / "dist" / "acorn.mjs",
          baseDir / "node_modules" / "acorn" / "package.json",
        )

        def copyFileIfNewer(src: File, dest: File): Unit = {
          val srcMod = IO.getModifiedTimeOrZero(src)
          val destMod = IO.getModifiedTimeOrZero(dest)

          if(srcMod == 0 || destMod == 0 || srcMod > destMod)
            IO.copyFile(src, dest, preserveLastModified = true)
        }

        copyFileIfNewer(moduleExtractorFile, moduleExtractorResFile)

        val extraResFiles = extraFiles(resDir)

        extraFiles(moduleExtractorDir).zip(extraResFiles)
          .foreach { case (src, dest) => copyFileIfNewer(src, dest) }

        moduleExtractorResFile +: extraResFiles
      }.taskValue,
    )
  )
  .jsConfigure(
    _.enablePlugins(NpmUtil)
      .dependsOn(js_module_extractor)
      .settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .dependsOn(js_module_extractor)
      .settings(commonNodeSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-backend-js",
  )

lazy val backend_jsJVM = backend_js.jvm
lazy val backend_jsJS = backend_js.js
lazy val backend_jsNode = backend_js.node

lazy val backend_generic = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-backend-generic"))
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

    name := "argon-backend-generic",
  )

lazy val backend_genericJVM = backend_generic.jvm
lazy val backend_genericJS = backend_generic.js
lazy val backend_genericNode = backend_generic.node

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


lazy val arstream = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-stream"))
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

    name := "argon-stream",
  )

lazy val arstreamJVM = arstream.jvm
lazy val arstreamJS = arstream.js
lazy val arstreamNode = arstream.node

lazy val argonio = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-io"))
  .dependsOn(arstream, util)
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

lazy val argonioJVM = argonio.jvm
lazy val argonioJS = argonio.js
lazy val argonioNode = argonio.node

lazy val argon_platform = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-platform"))
  .dependsOn(argonio)
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
  .platformsSettings(JVMPlatform, NodePlatform)(
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion % "optional",
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-platform",
  )

lazy val argon_platformJVM = argon_platform.jvm
lazy val argon_platformJS = argon_platform.js
lazy val argon_platformNode = argon_platform.node


lazy val modulefmt = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-modulefmt"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
      .settings(
        javacOptions ++= Seq(
          "-encoding", "UTF-8",
          "--release", "11",
          "-Xlint:-deprecation",
        ),

        Compile / PB.targets += scalapb.gen() -> (Compile / sourceManaged).value,
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
  .platformsSettings(JSPlatform, NodePlatform)(
    Compile / PB.targets += scalapb.gen() -> (Compile / sourceManaged).value / "protobuf",
  )
  .settings(
    commonSettings,

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-Xfatal-warnings",
      "-language:higherKinds",
      "-language:existentials",
      "-language:implicitConversions",
    ),

    Compile / PB.protoSources += file("argon-modulefmt/shared/src/main/protobuf"),
  )

lazy val modulefmtJVM = modulefmt.jvm
lazy val modulefmtJS = modulefmt.js

lazy val js_module_extractor = project.in(file("argon-js-module-extractor"))
  .enablePlugins(ScalaJSPlugin, NpmUtil)
  .settings(
    commonSettingsNoLibs,
    compilerOptions,

    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },

    npmDependencies ++= esParseDeps,

    name := "argon-js-module-extractor",
  )
