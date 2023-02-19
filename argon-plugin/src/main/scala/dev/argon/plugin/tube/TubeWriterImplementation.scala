package dev.argon.plugin.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ZipFileResource
import zio.*
import zio.stm.TMap

private[tube] object TubeWriterImplementation extends TubeWriterFactoryBase[true] {
  override protected def ifImplementation[A, B, C](value: A)(whenImplementation: A => C, whenInterface: Either[A, B] => C): C =
    whenImplementation(value)

  override protected def dummyImplementationValue: Unit = ()
}
