package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import dev.argon.util.ErrorWrapper
import dev.argon.options.*
import zio.*
import java.io.IOException
import scala.jdk.CollectionConverters.*
import scala.reflect.TypeTest

final class JavaPlugin[E >: IOException, EX <: Exception, TOptions, TOutput](inner: japi.Plugin[EX, TOptions, TOutput])(using Runtime[Any], ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends Plugin[E] {
  type Options = TOptions
  type Output = TOutput

  val optionHandler: OptionHandler[E, Options] = 
    inner.optionHandler() match {
      case optHandler: japi.options.OptionHandler[EX, Options, handler] =>
        new JavaOptionHandler[E, EX, Options, handler](optHandler)
    }
    
  val outputHandler: OutputHandler[E, Output] =
    inner.outputHandler() match {
      case outHandler: japi.options.OutputHandler[EX, Output] =>
        new JavaOutputHandler[E, EX, Output](outHandler)
    }
    

  def backend: IO[E, Backend[E, Options, Output]] =
    ZIO.attemptBlockingInterrupt {
      new JavaBackend[E, EX, Options, Output](inner.backend())
    }.catchAll(JavaErrorHandler.handleErrors[E, EX])

  def tubeLoaders: IO[E, Seq[TubeLoader[E, Options]]] =
    ZIO.attemptBlockingInterrupt {
      inner.tubeLoaders().asScala.toSeq
    }
      .catchAll(JavaErrorHandler.handleErrors[E, EX])
      .flatMap { loaders =>
        ZIO.runtime.map { runtime =>
          given Runtime[Any] = runtime
          loaders.map(new JavaTubeLoader(_))
        }
        
      }

  def buildOutputExecutor: IO[E, Option[BuildOutputExecutor[Output]]] =
    ZIO.attemptBlockingInterrupt {
      Option(inner.outputExecutor())
    }
      .catchAll(JavaErrorHandler.handleErrors[E, EX])
      .flatMap { executor =>
        ZIO.runtime.map { runtime =>
          given Runtime[Any] = runtime
          executor.map(new JavaBuildOutputExecutors(_))
        }
      }


}
