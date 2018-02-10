name := "argon"

scalaVersion := "2.12.4"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.11"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.11"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.5"
libraryDependencies += "commons-io" % "commons-io" % "2.5"
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "com.fommil" %% "deriving-macro" % "0.9.0"
libraryDependencies += "com.fommil" %% "scalaz-deriving" % "0.9.0"

libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused:-implicits,-explicits",
  "-Ypatmat-exhaust-depth", "500",
  "-feature",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
)

scalacOptions in (Compile, compile) += "-Xfatal-warnings"

scalacOptions in (Compile, console) ~= (_ filterNot (opt => opt == "-Xlint"))
scalacOptions in (Test, console) ~= (_ filterNot (opt => opt == "-Xlint"))

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
)

test in assembly := {}
assemblyJarName in assembly := "argon.jar"
