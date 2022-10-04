package dev.argon.platform

import zio.*
import typings.node.processMod.global.process as NodeProcess
import typings.node.osMod as NodeOS
import scala.scalajs.js

private[platform] object NodeSystemLayer {

  private def nodeEnv: js.Dictionary[String] =
    NodeProcess.env.asInstanceOf[js.Dictionary[String]]

  def live: ZLayer[Any, Nothing, System] =
    ZLayer.succeed(new System {
      override def env(variable: => String)(implicit trace: Trace): IO[SecurityException, Option[String]] =
        ZIO.succeed { nodeEnv.get(variable) }

      override def envOrElse(variable: => String, alt: => String)(implicit trace: Trace): IO[SecurityException, String] =
        ZIO.succeed { nodeEnv.get(variable).getOrElse(alt) }

      override def envOrOption(variable: => String, alt: => Option[String])(implicit trace: Trace): IO[SecurityException, Option[String]] =
        ZIO.succeed { nodeEnv.get(variable).orElse(alt) }

      override def envs(implicit trace: Trace): IO[SecurityException, Map[String, String]] =
        ZIO.succeed { nodeEnv.toMap }

      override def lineSeparator(implicit trace: Trace): UIO[String] =
        ZIO.succeed { NodeOS.EOL }

      override def properties(implicit trace: Trace): IO[Throwable, Map[String, String]] =
        ZIO.succeed { Map.empty }

      override def property(prop: => String)(implicit trace: Trace): IO[Throwable, Option[String]] =
        ZIO.none

      override def propertyOrElse(prop: => String, alt: => String)(implicit trace: Trace): IO[Throwable, String] =
        ZIO.succeed { alt }

      override def propertyOrOption(prop: => String, alt: => Option[String])(implicit trace: Trace): IO[Throwable, Option[String]] =
        ZIO.succeed { alt }
    })

}
