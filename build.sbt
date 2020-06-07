import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.internal.util.ManagedLogger
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

val graalVersion = "20.1.0"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

val zioVersion = "1.0.0-RC20"

val esParseDeps = Seq(
  "escodegen" -> "^1.14.1",
  "esprima" -> "^4.0.1",
)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.2",

  resolvers += Resolver.sonatypeRepo("releases"),

  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-M1",

    "org.typelevel" %%% "cats-core" % "2.1.1",
    "org.typelevel" %%% "kittens" % "2.1.0",
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,
    "dev.zio" %%% "zio-interop-cats" % "2.1.3.0-RC15",


    "com.chuusai" %%% "shapeless" % "2.3.3",
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
  ),

)

lazy val sharedJVMNodeSettings = Seq(
  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared-jvm-node/src/main/scala",
  unmanagedSourceDirectories in Test += baseDirectory.value / "../shared-jvm-node/src/test/scala",
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.0.0",
  ),

  npmDependencies ++= Seq(
    "jszip" -> "^3.2.2",
    "xmldom" -> "^0.3.0",
  ) ++ esParseDeps,
  
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared-js-node/src/main/scala",
  unmanagedSourceDirectories in Test += baseDirectory.value / "../shared-js-node/src/test/scala",

)

lazy val commonJVMSettings = sharedJVMNodeSettings ++ Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-text" % "1.8",
    "org.apache.commons" % "commons-compress" % "1.20",
    "commons-io" % "commons-io" % "2.7",
  ),

  fork in Test := true,
  envVars in Test ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val commonNodeSettings = sharedJSNodeSettings ++ sharedJVMNodeSettings ++ Seq(

  npmDependencies ++= Seq(
    "memory-streams" -> "^0.1.3",
    "node-stream-zip" -> "^1.8.2",
  ),

  jsEnv := new NodeJSEnv(
    NodeJSEnv.Config()
      .withEnv(envValues)
      .withArgs(List("--no-warnings", "--experimental-vm-modules"))
  ),
)

lazy val zioEffectWarts = project.in(file("zio-effect-warts"))
  .settings(
    scalaVersion := "2.13.2",
    libraryDependencies ++= Seq(
      "org.wartremover" % "wartremover" % wartremover.Wart.PluginVersion cross CrossVersion.full,
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
    ),


  )

lazy val compilerOptions = Seq(

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-Wconf:cat=lint:wv," +
      "cat=deprecation:wv," +
      "cat=feature:wv," +
      "cat=optimizer:wv," +
      "cat=unchecked&msg=The outer reference in this type test cannot be checked at run time\\.:silent," +
      "cat=unchecked:wv," +
      "cat=java-source:wv," +
      "cat=unused-imports:silent," +
      "cat=unused:wv," +
      "cat=w-flag-dead-code:silent," +
      "cat=w-flag:wv," +
      "cat=other-match-analysis&msg=unreachable code:silent",
    "-Xfatal-warnings",
    "-Ypatmat-exhaust-depth", "500",
    "-Yrangepos",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
  ),
  
  scalacOptions in (Compile, console) ~= (_ filterNot (opt => opt == "-Xlint")),
  scalacOptions in (Test, console) ~= (_ filterNot (opt => opt == "-Xlint")),


  wartremoverWarnings ++= Warts.allBut(
    Wart.Recursion,
    Wart.Any,
    Wart.Nothing,
    Wart.Product,
    Wart.Serializable,
    Wart.JavaSerializable,
    Wart.LeakingSealed,
    Wart.Overloading,
    Wart.ImplicitConversion,
    Wart.ImplicitParameter,
    Wart.ExplicitImplicitTypes,
    Wart.DefaultArguments,
    Wart.PublicInference,

    Wart.Throw,
  ) ++ Seq(
    Wart.custom("dev.argon.warts.ZioEffect"),
  ),

  wartremoverExcluded += sourceManaged.value,

  wartremoverClasspaths ++= {
    (fullClasspath in (zioEffectWarts, Compile)).value.map(_.data.toURI.toString)
  }
)

lazy val buildArgonLibs = taskKey[Unit]("Compile Argon libraries")
lazy val parcelJS = taskKey[File]("Parcel version of JS output")


lazy val cli = crossProject(JVMPlatform, NodePlatform).in(file("argon-cli"))
  .dependsOn(argon_build, argon_platform)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .nodeConfigure(
    _.enablePlugins(NpmUtil)
      .settings(
        commonNodeSettings,

        scalaJSUseMainModuleInitializer := true,

    )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-cli",

    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",

    buildArgonLibs := (Def.taskDyn {
      def buildLibTask(libName: String): Def.Initialize[Task[Unit]] =
        Def.task {
          val r = (Compile / runner).value
          val libDir = file("libraries") / libName
          val classpath = (Compile / fullClasspath).value

          streams.value.log.info(s"Building library $libName")

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
            streams.value.log.info(s"Building library $libName ($backend)")
            Run.run(
              "dev.argon.Program",
              classpath.map { _.data },
              Seq(
                "build",
                backend,

                "--moduleName",
                libName,
              ) ++
                inputFileOpts ++
                opts,
              streams.value.log
            )(r).get
          }
        }

      Def.sequential(
        Seq(
          buildLibTask("Argon.Core"),
        ),

        Def.task {}
      )
    }).value
  )

lazy val cliJVM = cli.jvm
lazy val cliNode = cli.node

lazy val webDemo = project.in(file("argon-web-demo"))
  .enablePlugins(ScalaJSPlugin, NpmUtil)
  .dependsOn(argon_buildJS, argon_platformJS)
  .settings(
    commonSettings,
    commonBrowserSettings,
    compilerOptions,

    name := "argon-web-demo",

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  )

lazy val argon_build = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-build"))
  .dependsOn(argon_build_base, backend_js, backend_module, argon_platform % "test")
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
  .dependsOn(parser, backend_common)
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
  .dependsOn(argonio, modulefmt, parser_data)
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

lazy val backend_common = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-backend-common"))
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

    name := "argon-backend-common",
  )

lazy val backend_commonJVM = backend_common.jvm
lazy val backend_commonJS = backend_common.js
lazy val backend_commonNode = backend_common.node

lazy val armodule_loader = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-armodule-loader"))
  .dependsOn(argon_compiler, backend_common)
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

    name := "argon-armodule-loader",
  )

lazy val armodule_loaderJVM = armodule_loader.jvm
lazy val armodule_loaderJS = armodule_loader.js
lazy val armodule_loaderNode = armodule_loader.node


lazy val backend_js = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-backend-js"))
  .dependsOn(armodule_loader)
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
        val bundledFile = (js_module_extractor / parcelJS).value
        val resourceFile = (Compile / resourceManaged).value / "js-module-extractor.js"

        val bundledFileMod = IO.getModifiedTimeOrZero(bundledFile)
        val resourceFileMod = IO.getModifiedTimeOrZero(resourceFile)

        if(bundledFileMod == 0 || resourceFileMod == 0 || bundledFileMod > resourceFileMod)
          IO.copyFile(bundledFile, resourceFile, preserveLastModified = true)

        Seq(resourceFile)
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

    name := "argon-compiler-js",
  )

lazy val backend_jsJVM = backend_js.jvm
lazy val backend_jsJS = backend_js.js
lazy val backend_jsNode = backend_js.node

lazy val backend_module = crossProject(JVMPlatform, JSPlatform, NodePlatform).in(file("argon-backend-module"))
  .dependsOn(armodule_loader)
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

    name := "argon-compiler-module",
  )

lazy val backend_moduleJVM = backend_module.jvm
lazy val backend_moduleJS = backend_module.js
lazy val backend_moduleNode = backend_module.node

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
  .dependsOn(arstream)
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

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings",
      "-Ypatmat-exhaust-depth", "500",
      "-language:higherKinds",
      "-language:existentials",
      "-language:implicitConversions",
    ),

    PB.protoSources in Compile := Seq(file("argon-modulefmt/src/main/protobuf")),

    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
    ),
  )

lazy val modulefmtJVM = modulefmt.jvm
lazy val modulefmtJS = modulefmt.js

lazy val js_module_extractor = project.in(file("argon-js-module-extractor"))
  .enablePlugins(ScalaJSPlugin, NpmUtil)
  .settings(
    scalaVersion := "2.13.2",

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings",
      "-Ypatmat-exhaust-depth", "500",
      "-language:higherKinds",
      "-language:existentials",
      "-language:implicitConversions",
    ),

    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    npmDependencies ++= esParseDeps,

    npmDevDependencies += "parcel-bundler" -> "^1.12.4",

    parcelJS := {
      npmInstall.value

      val dir = crossTarget.value
      val scalaJSOutput = (Compile / fullOptJS).value.data.file
      val bundledOutput = dir / "dist" / scalaJSOutput.getName

      FileFunction.cached(streams.value.cacheDirectory / "parcel-bundle") { _ =>
        import scala.sys.process.Process
        Process("node_modules/.bin/parcel" :: "build" :: "--global" :: "ModuleExtractor" :: scalaJSOutput.toString :: Nil, dir).!
        Set(bundledOutput)
      }(Set(scalaJSOutput))

      bundledOutput
    },

    name := "argon-js-module-extractor",
  )
