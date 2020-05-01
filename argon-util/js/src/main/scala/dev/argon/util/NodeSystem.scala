package dev.argon.util

import zio._
import zio.system._

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
object NodeSystem {

  val nodeLive: ZLayer[Any, Nothing, System] =
    ZLayer.succeed(new System.Service {
      override def env(variable: String): IO[SecurityException, Option[String]] =
        IO.effectTotal { NodeProcess.env(variable).toOption }

      override def property(prop: String): IO[Throwable, Option[String]] =
        IO.succeed(None)

      override def lineSeparator: UIO[String] = IO.succeed(NodeOS.EOL)
    })

}
