import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.internal.util.ManagedLogger
import org.scalajs.jsenv.nodejs.NodeJSEnv

val graalVersion = "20.0.0"

lazy val envValues = Map(
  "ARGON_LIB_DIR" -> file("libraries").getAbsolutePath,
  "ARGON_TEST_CASES" -> file("testcases").getAbsolutePath,
)

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
    "dev.zio" %%% "zio" % "1.0.0-RC18-2",
    "dev.zio" %%% "zio-streams" % "1.0.0-RC18-2",
    "dev.zio" %%% "zio-interop-cats" % "2.0.0.0-RC12",


    "com.chuusai" %%% "shapeless" % "2.3.3",
    "tech.sparse" %%%  "toml-scala" % "0.2.2",
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
    "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

    "dev.zio" %%% "zio-test" % "1.0.0-RC18-2" % "test",
    "dev.zio" %%% "zio-test-sbt" % "1.0.0-RC18-2" % "test",

    compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.6.0" cross CrossVersion.full),
    "com.github.ghik" %%% "silencer-lib" % "1.6.0" % Provided cross CrossVersion.full,
  ),

)

lazy val commonJVMSettings = Seq(

  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-text" % "1.8",
    "org.apache.commons" % "commons-compress" % "1.20",
    "commons-io" % "commons-io" % "2.6",
  ),

  fork in Test := true,
  envVars in Test ++= envValues,

)

lazy val commonJSSettings = Seq(

  libraryDependencies ++= Seq(
    "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3",
  ),

  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

  jsEnv := new NodeJSEnv(
    NodeJSEnv.Config()
      .withEnv(envValues)
      .withArgs(List("--no-warnings", "--experimental-vm-modules"))
  ),

  scalacOptions ++= Seq(
    "-P:scalajs:sjsDefinedByDefault",
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


  wartremoverWarnings in (Compile, compile) ++= Warts.allBut(
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
  ),

  wartremoverExcluded += sourceManaged.value,
)

lazy val buildArgonLibs = taskKey[Unit]("Compile Argon libraries")

lazy val cli = crossProject(JVMPlatform, JSPlatform).in(file("argon-cli"))
  .dependsOn(argon_build)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(
      commonJSSettings,

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
lazy val cliJS = cli.js

lazy val webDemo = project.in(file("argon-web-demo"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(argon_buildJS)
  .settings(
    commonSettings,
    commonJSSettings,
    compilerOptions,

    name := "argon-web-demo",

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  )

lazy val argon_build = crossProject(JVMPlatform, JSPlatform).in(file("argon-build"))
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
    _.settings(commonJSSettings)
  )
  .dependsOn(arstream, util, parser, argon_compiler, backend_js, backend_module)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-build",
    wartremoverExcluded += sourceDirectory.value / "main" / "scala" / "dev" / "argon" / "build" / "testrunner" / "js" / "api.gen.scala",
  )

lazy val argon_buildJVM = argon_build.jvm
lazy val argon_buildJS = argon_build.js

lazy val grammar = crossProject(JVMPlatform, JSPlatform).in(file("argon-grammar"))
  .dependsOn(arstream, util)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-grammar",
  )

lazy val grammarJVM = grammar.jvm
lazy val grammarJS = grammar.js

lazy val parser = crossProject(JVMPlatform, JSPlatform).in(file("argon-parser"))
  .dependsOn(arstream, util, parser_data, grammar)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-parser",
  )

lazy val parserJVM = parser.jvm
lazy val parserJS = parser.js

lazy val parser_data = crossProject(JVMPlatform, JSPlatform).in(file("argon-parser-data"))
  .dependsOn(arstream, util, grammar)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-parser-data",
  )

lazy val parser_dataJVM = parser_data.jvm
lazy val parser_dataJS = parser_data.js

lazy val argon_compiler = crossProject(JVMPlatform, JSPlatform).in(file("argon-compiler"))
  .dependsOn(arstream, util, argonio, modulefmt, parser_data)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler",
  )

lazy val argon_compilerJVM = argon_compiler.jvm
lazy val argon_compilerJS = argon_compiler.js


lazy val backend_js = crossProject(JVMPlatform, JSPlatform).in(file("argon-backend-js"))
  .dependsOn(arstream, util, modulefmt, parser_data, argon_compiler)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler-js",
  )

lazy val backend_jsJVM = backend_js.jvm
lazy val backend_jsJS = backend_js.js

lazy val backend_module = crossProject(JVMPlatform, JSPlatform).in(file("argon-backend-module"))
  .dependsOn(arstream, util, modulefmt, parser_data, argon_compiler)
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler-module",
  )

lazy val backend_moduleJVM = backend_module.jvm
lazy val backend_moduleJS = backend_module.js

lazy val util = crossProject(JVMPlatform, JSPlatform).in(file("argon-util"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-util",
  )

lazy val utilJVM = util.jvm
lazy val utilJS = util.js

lazy val arstream = crossProject(JVMPlatform, JSPlatform).in(file("argon-stream"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-stream",
  )

lazy val arstreamJVM = arstream.jvm
lazy val arstreamJS = arstream.js

lazy val argonio = crossProject(JVMPlatform, JSPlatform).in(file("argon-io"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
  )
  .dependsOn(arstream, util)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-io",
  )

lazy val argonioJVM = argonio.jvm
lazy val argonioJS = argonio.js

lazy val modulefmt = crossProject(JVMPlatform, JSPlatform).in(file("argon-modulefmt"))
  .jvmConfigure(
    _.settings(commonJVMSettings)
  )
  .jsConfigure(
    _.settings(commonJSSettings)
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


