package dev.argon.plugin

import dev.argon.io.*
import dev.argon.plugin.{api => japi}
import zio.*
import dev.argon.util.ErrorWrapper
import scala.jdk.CollectionConverters.*
import java.io.IOException
import scala.reflect.TypeTest

final class JavaTubeLoader[E >: IOException, EX <: Exception, Options](inner: japi.TubeLoader[EX, Options])(using Runtime[Any], ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends TubeLoader[E, Options] {
  
  override def load(options: Options)(resource: BinaryResource[E]): Managed[E, SerializedTube[E]] =
    ZManaged.acquireReleaseWith(
      IO.attemptBlockingInterrupt {
        inner.load(options, new JavaBinaryResourceWrap[E, EX](resource))
      }
        .catchAll(JavaErrorHandler.handleErrors[E, EX])
    )(tube =>
      IO.succeedBlocking {
        tube.close()
      }
    ).map { tube => new JavaSerializedTubeUnwrap[E, EX](tube.tube()) }
    
      
    

  val supportedExtensions: Seq[String] =
    inner.supportedExtensions.asScala.toSeq
}
