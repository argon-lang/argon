package dev.argon.backend.js

final case class JSBackendOptions[F[_], I]
(
  extern: F[Map[String, String]],
  inject: F[JSInjectCode[F]],
)

final case class JSInjectCode[F[_]]
(
  before: F[Option[String]],
  after: F[Option[String]],
)

final case class JSOutputOptions[F[_], I]
(
  outputFile: F[I],
)
