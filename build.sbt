import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.internal.util.ManagedLogger
import org.scalajs.jsenv.nodejs.NodeJSEnv
import NodePlatformImplicits._

val graalVersion = "20.0.0"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

val zioVersion = "1.0.0-RC18-2"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.0",

  resolvers += Resolver.sonatypeRepo("releases"),

  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-M1",

    "org.typelevel" %%% "cats-core" % "2.1.1",
    "org.typelevel" %%% "cats-effect" % "2.1.3",
    "org.typelevel" %%% "kittens" % "2.0.0",
    "dev.zio" %%% "zio" % zioVersion,
    "dev.zio" %%% "zio-streams" % zioVersion,
    "dev.zio" %%% "zio-interop-cats" % "2.0.0.0-RC12",


    "com.chuusai" %%% "shapeless" % "2.3.3",
    "tech.sparse" %%%  "toml-scala" % "0.2.2",
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

    "dev.zio" %%% "zio-test" % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",

    compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.6.0" cross CrossVersion.full),
    "com.github.ghik" %%% "silencer-lib" % "1.6.0" % Provided cross CrossVersion.full,
  ),

)

lazy val sharedJVMNodeSettings = Seq(
  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared-jvm-node/src/main/scala",
  unmanagedSourceDirectories in Test += baseDirectory.value / "../shared-jvm-node/src/test/scala",
)

lazy val sharedJSNodeSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3",
  ),

  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

  scalacOptions ++= Seq(
    "-P:scalajs:sjsDefinedByDefault",
  ),

  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared-js-node/src/main/scala",
  unmanagedSourceDirectories in Test += baseDirectory.value / "../shared-js-node/src/test/scala",

)

lazy val commonJVMSettings = sharedJVMNodeSettings ++ Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-text" % "1.8",
    "org.apache.commons" % "commons-compress" % "1.20",
    "commons-io" % "commons-io" % "2.6",
  ),

  fork in Test := true,
  envVars in Test ++= envValues,

)

lazy val commonBrowserSettings = sharedJSNodeSettings

lazy val commonNodeSettings = sharedJSNodeSettings ++ sharedJVMNodeSettings ++ Seq(
  jsEnv := new NodeJSEnv(
    NodeJSEnv.Config()
      .withEnv(envValues)
      .withArgs(List("--no-warnings", "--experimental-vm-modules"))
  ),
)

lazy val zioEffectWarts = project.in(file("zio-effect-warts"))
  .settings(
    scalaVersion := "2.13.0",
    libraryDependencies ++= Seq(
      "org.wartremover" % "wartremover" % wartremover.Wart.PluginVersion cross CrossVersion.full,
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
    ),


  )

lazy val compilerOptions = Seq(

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-Xfatal-warnings",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused:-implicits,-explicits,-imports",
    "-Ypatmat-exhaust-depth", "500",
    "-Yrangepos",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-P:silencer:globalFilters=unreachable;outer reference in this type test cannot be checked at run time.",
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

lazy val cli = crossProject(JVMPlatform, NodePlatform).in(file("argon-cli"))
  .dependsOn(argon_build, argon_platform)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .nodeConfigure(
    _.settings(
      commonNodeSettings,

      scalaJSUseMainModuleInitializer := true,

    )
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-cli",


    buildArgonLibs := (Def.taskDyn {
      Def.sequential(
        IO.listFiles(file("libraries"), DirectoryFilter).map { libDir =>
          val libName = libDir.getName

          Def.task {
            streams.value.log.info(s"Building library $libName")
            (runMain in Compile).toTask(s" dev.argon.Program compile libraries/$libName/build.toml").value
          }
        },

        Def.task {}
      )
    }).value
  )

lazy val cliJVM = cli.jvm
lazy val cliNode = cli.node

lazy val webDemo = project.in(file("argon-web-demo"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(argon_buildJS, argon_platformJS)
  .settings(
    commonSettings,
    commonBrowserSettings,
    compilerOptions,

    name := "argon-web-demo",

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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
    _.settings(commonBrowserSettings)
  )
  .nodeConfigure(
    _.settings(commonNodeSettings)
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

