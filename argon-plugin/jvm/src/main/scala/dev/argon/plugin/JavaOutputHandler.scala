package dev.argon.plugin

import dev.argon.options.*
import dev.argon.plugin.{api => japi}
import dev.argon.util.ErrorWrapper
import dev.argon.io.Resource
import scala.jdk.CollectionConverters.*
import java.io.IOException
import scala.reflect.TypeTest

final class JavaOutputHandler[E >: IOException, EX <: Exception, Output](inner: japi.options.OutputHandler[EX, Output])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends OutputHandler[E, Output] {
  override val options: Set[OutputInfo[_ <: Resource[E], Output]] =
    inner.options.asScala.iterator.map(createOutputInfo).toSet

  private def createOutputInfo[A <: japi.resource.Resource[EX]](inner: japi.options.OutputInfo[A, Output]): OutputInfo[Resource[E], Output] =
    new OutputInfo[Resource[E], Output] {
      override val name: String = inner.name
      override val description: String = inner.description

      override def getValue(options: Output): Resource[E] =
        inner.getValue(options) match {
          case resource: japi.resource.BinaryResource[EX] =>
            new JavaBinaryResourceUnwrap[E, EX](resource)

          case resource: japi.resource.DirectoryResource[EX] =>
            new JavaDirectoryResourceUnwrap[E, EX](resource)

          case resource => throw new MatchError(resource)
        }
    }

}
