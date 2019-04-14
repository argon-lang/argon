package com.mi3software.argon.compiler.js

final case class JSBackendOptions[F[_], I]
(
  outputFile: F[I],
  extern: F[Map[String, String]]
)
