package com.mi3software.argon.compiler.js

final case class JSBackendOptions[F[_], I]
(
  outputFile: F[I],
  extern: F[Map[String, String]],
  inject: F[JSInjectCode[F]],
)

final case class JSInjectCode[F[_]]
(
  before: F[Option[String]],
  after: F[Option[String]],
)
