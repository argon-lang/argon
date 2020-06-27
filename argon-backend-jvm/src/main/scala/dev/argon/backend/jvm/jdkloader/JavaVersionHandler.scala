package dev.argon.backend.jvm.jdkloader

import cats.implicits._
import zio._
import zio.system._

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
object JavaVersionHandler {


  def isMinJDKVersion(version: Int): URIO[System, Boolean] =
    checkJdkVersion { _ >= version }

  def isCurrentJDKVersion(version: Int): URIO[System, Boolean] =
    checkJdkVersion { _ === version }


  private def parseMajorVersion(version: String): IO[NumberFormatException, Int] = {
    val dot = version.indexOf(".")
    if(dot < 0) IO.effect { version.toInt }.refineToOrDie[NumberFormatException]
    else {
      val majorStr = version.substring(0, dot)
      IO.effect { majorStr.toInt }.refineToOrDie[NumberFormatException].flatMap {
        case major if major === 1 =>
          val dot2 = version.indexOf(".", dot + 1)
          val minorStr =
            if(dot2 < 0) version.substring(dot + 1)
            else version.substring(dot + 1, dot2)

          IO.effect { minorStr.toInt }.refineToOrDie[NumberFormatException]

        case major => IO.succeed(major)
      }
    }
  }

  private def getJdkVersion: ZIO[System, NumberFormatException, Option[Int]] =
    property("java.version").orDie.flatMap { ver => ZIO.foreach(ver)(parseMajorVersion) }

  private def checkJdkVersion(f: Int => Boolean): URIO[System, Boolean] =
    getJdkVersion.map { _.exists(f) }.catchAll { _ => IO.succeed(false) }

}
