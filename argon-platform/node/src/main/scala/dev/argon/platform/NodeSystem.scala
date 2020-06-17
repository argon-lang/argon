package dev.argon.platform

import zio._
import zio.system._

import scala.scalajs.js

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] object NodeSystem {

  val live: ZLayer[Any, Nothing, System] =
    ZLayer.succeed(new System.Service {
      override def env(variable: String): IO[SecurityException, Option[String]] =
        IO.effectTotal { NodeProcess.env(variable).toOption }

      override def envOrElse(variable: String, alt: => String): IO[SecurityException, String] =
        env(variable).map { _.getOrElse(alt) }

      override def envOrOption(variable: String, alt: => Option[String]): IO[SecurityException, Option[String]] =
        env(variable).map { _.orElse(alt) }

      override def envs: IO[SecurityException, Map[String, String]] =
        IO.effectTotal {
          js.Object.keys(NodeProcess.env)
            .map { key => key -> NodeProcess.env(key).toOption }
            .collect {
              case (key, Some(value)) => key -> value
            }
            .toMap
        }

      override def property(prop: String): IO[Throwable, Option[String]] =
        IO.none

      override def properties: IO[Throwable, Map[String, String]] =
        IO.succeed(Map.empty)

      override def propertyOrElse(prop: String, alt: => String): IO[Throwable, String] =
        IO.succeed(alt)

      override def propertyOrOption(prop: String, alt: => Option[String]): IO[Throwable, Option[String]] =
        IO.succeed(alt)

      override def lineSeparator: UIO[String] = IO.succeed(NodeOS.EOL)
    })

}
