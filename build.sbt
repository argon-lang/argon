name := "argon"

scalaVersion := "2.12.4"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.11"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.11"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.5"
libraryDependencies += "commons-io" % "commons-io" % "2.5"
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"

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
  "-Ypatmat-exhaust-depth", "160",
  "-feature",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
)


wartremoverWarnings ++= Warts.allBut(
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

  Wart.TraversableOps,
)

unmanagedJars in Compile += baseDirectory.value / "thrift-repo/lib/java/build/libthrift-1.0.0.jar"

test in assembly := {}
assemblyJarName in assembly := "argon.jar"
