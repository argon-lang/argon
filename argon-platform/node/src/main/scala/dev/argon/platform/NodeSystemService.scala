package dev.argon.platform

import zio.*
import scala.scalajs.js

private[platform] class NodeSystemService extends System {
  override def env(variable: => String)(using trace: Trace): IO[SecurityException, Option[String]] =
    ZIO.succeed { NodeProcess.env.get(variable) }

  override def envOrElse(variable: => String, alt: => String)(using trace: Trace): IO[SecurityException, String] =
    ZIO.succeed { NodeProcess.env.get(variable).getOrElse(alt) }

  override def envOrOption(variable: => String, alt: => Option[String])(using trace: Trace): IO[SecurityException, Option[String]] =
    ZIO.succeed { NodeProcess.env.get(variable).orElse(alt) }

  override def envs(using trace: Trace): IO[SecurityException, Map[String, String]] =
    ZIO.succeed { NodeProcess.env.toMap }

  override def lineSeparator(using trace: Trace): UIO[String] =
    ZIO.succeed { NodeOS.EOL }

  override def properties(using trace: Trace): IO[Throwable, Map[String, String]] =
    ZIO.succeed { Map.empty }

  override def property(prop: => String)(using trace: Trace): IO[Throwable, Option[String]] =
    ZIO.none

  override def propertyOrElse(prop: => String, alt: => String)(using trace: Trace): IO[Throwable, String] =
    ZIO.succeed { alt }

  override def propertyOrOption(prop: => String, alt: => Option[String])(using trace: Trace): IO[Throwable, Option[String]] =
    ZIO.succeed { alt }
}

private[platform] object NodeSystemService {

  def live: ZLayer[Any, Nothing, System] =
    ZLayer.scoped({
      val system = NodeSystemService()
      ZIO.withSystemScoped(system).as(system)
    })

}
