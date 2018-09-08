

lazy val commonSettings = Seq(
  scalaVersion := "2.12.6",

  resolvers += Resolver.sonatypeRepo("releases"),

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),

  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    "org.scalaz" %% "scalaz-core" % "7.2.11",
    "org.scalaz" %% "scalaz-effect" % "7.2.11",
    "org.apache.commons" % "commons-lang3" % "3.5",
    "commons-io" % "commons-io" % "2.5",
    "org.fusesource.jansi" % "jansi" % "1.15",
    "org.scalatest" %% "scalatest" % "3.0.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.fommil" %% "deriving-macro" % "0.9.0",
    "com.fommil" %% "scalaz-deriving" % "0.9.0",
    "org.json4s" %% "json4s-native" % "3.5.4",
    "co.fs2" %% "fs2-core" % "0.10.1",
    "co.fs2" %% "fs2-io" % "0.10.1",
    "com.codecommit" %% "shims" % "1.4.0",
    "com.codecommit" %% "shims-effect" % "1.4.0",

    "org.apache.thrift" % "libthrift" % "0.11.0",
    "com.twitter" %% "scrooge-core" % "18.7.0" exclude("com.twitter", "libthrift"),

    "com.thoughtworks.each" %% "each" % "3.3.1",

    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test",
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
  ),

  scalacOptions in (Compile, compile) += "-Xfatal-warnings",

  scalacOptions in (Compile, console) ~= (_ filterNot (opt => opt == "-Xlint")),
  scalacOptions in (Test, console) ~= (_ filterNot (opt => opt == "-Xlint")),
)

lazy val wartremoverOptions = Seq(
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

    Wart.TraversableOps,
  ),

  wartremoverExcluded += sourceManaged.value,
)

val generateLibs = taskKey[Unit]("Generate argon library definitions")

lazy val cli = project.in(file("argon-cli"))
  .dependsOn(argon_build)
  .settings(
    commonSettings,
    compilerOptions,
    wartremoverOptions,

    name := "argon-cli",
  )

lazy val argon_build = project.in(file("argon-build"))
  .dependsOn(util, parser, argon_compiler, compiler_js)
  .settings(
    commonSettings,
    compilerOptions,
    wartremoverOptions,

    name := "argon-build",
  )

lazy val parser = project.in(file("argon-parser"))
  .dependsOn(util, parser_data)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-parser",
  )

lazy val parser_data = project.in(file("argon-parser-data"))
  .dependsOn(util)
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
    wartremoverExcluded += sourceDirectory.value / "main/scala/com/mi3software/argon/compiler/ArgonTypeComparer.scala"
  )

lazy val compiler_js = project.in(file("argon-compiler-js"))
  .dependsOn(util, modulefmt, parser_data, argon_compiler)
  .settings(
    commonSettings,
    compilerOptions,

    name := "argon-compiler-js",
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

    scroogeBuildOptions := Seq(),
  )

lazy val library_gen = project.in(file("argon-library-gen"))
  .dependsOn(modulefmt)
  .settings(
    commonSettings,
    compilerOptions,

    libraryDependencies += "com.lihaoyi" % "ammonite" % "1.1.2" cross CrossVersion.patch,

    name := "argon-library-gen",
  )


