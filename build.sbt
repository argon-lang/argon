

lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",

  resolvers += Resolver.sonatypeRepo("releases"),

  //addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
  addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.3.3"),
  addCompilerPlugin("org.scalaz" %% "deriving-plugin" % "1.0.0"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
    
    "org.scalaz" %% "scalaz-core" % "7.2.27",
    "org.scalaz" %% "scalaz-zio" % "1.0-RC3",
    "org.scalaz" %% "scalaz-zio-interop-scalaz7x" % "1.0-RC3",
    "org.scalaz" %% "deriving-macro" % "1.0.0",
    "org.scalaz" %% "scalaz-deriving" % "1.0.0",

    "com.chuusai" %% "shapeless" % "2.3.3",
    "tech.sparse" %%  "toml-scala" % "0.2.0",
    
    "org.apache.commons" % "commons-lang3" % "3.8.1",
    "commons-io" % "commons-io" % "2.6",
    
    "org.fusesource.jansi" % "jansi" % "1.18",
    
    "org.scalatest" %% "scalatest" % "3.0.7" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test",

    "com.github.ghik" %% "silencer-lib" % "1.2" % Provided,
  )

)

lazy val compilerOptions = Seq(

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xfuture",
    "-Xlint",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused:-implicits,-explicits,-imports",
    "-Ypatmat-exhaust-depth", "500",
    "-Ypartial-unification",
    "-Yrangepos",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-P:silencer:globalFilters=unreachable;outer reference in this type test cannot be checked at run time.",
  ),

  scalacOptions in (Compile, compile) += "-Xfatal-warnings",

  scalacOptions in (Compile, console) ~= (_ filterNot (opt => opt == "-Xlint")),
  scalacOptions in (Test, console) ~= (_ filterNot (opt => opt == "-Xlint")),


  wartremoverWarnings in (Compile, compile) ++= Warts.allBut(
    Wart.Recursion,
    Wart.Any,
    Wart.Nothing,
    Wart.Product,
    Wart.Serializable,
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

lazy val gcrpcRuntime = RootProject(file("gcrpc/runtime/scala"))

lazy val cli = project.in(file("argon-cli"))
  .dependsOn(argon_build)
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
            (runMain in Compile).toTask(s" com.mi3software.argon.Program compile libraries/$libName/build.toml").value
          }
        },

        Def.task {}
      )
    }).value
  )

lazy val argon_build = project.in(file("argon-build"))
  .dependsOn(util, parser, argon_compiler, compiler_js, compiler_module, gcrpcRuntime)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-build",
  )

lazy val grammar = project.in(file("argon-grammar"))
  .dependsOn(util)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-grammar",
  )

lazy val parser = project.in(file("argon-parser"))
  .dependsOn(util, parser_data, grammar)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-parser",
  )

lazy val parser_data = project.in(file("argon-parser-data"))
  .dependsOn(util, grammar)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-parser-data",
  )

lazy val argon_compiler = project.in(file("argon-compiler"))
  .dependsOn(util, modulefmt, parser_data)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler",
  )

lazy val compiler_js = project.in(file("argon-compiler-js"))
  .dependsOn(util, modulefmt, parser_data, argon_compiler)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler-js",
  )

lazy val compiler_module = project.in(file("argon-compiler-module"))
  .dependsOn(util, modulefmt, parser_data, argon_compiler)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler-module",
  )

lazy val util = project.in(file("argon-util"))
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-util",
  )

lazy val modulefmt = project.in(file("argon-modulefmt"))
  .settings(
    commonSettings,

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Xfatal-warnings",
      "-Ypatmat-exhaust-depth", "500",
      "-language:higherKinds",
      "-language:existentials",
      "-language:implicitConversions",
    ),

    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
    
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
    ),
  )

