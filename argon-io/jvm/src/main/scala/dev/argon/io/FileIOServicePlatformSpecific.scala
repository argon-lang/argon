package dev.argon.io

import java.io.IOException

import zio.Managed
import java.lang.Runtime.{Version => JDKVersion}

trait FileIOServicePlatformSpecific[P] {
  def openJarFile[R, E](errorHandler: IOException => E)(path: P, jdkVersion: JDKVersion): Managed[E, JarFileReader[R, E]]
}
