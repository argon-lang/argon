package dev.argon.compiler_tests

import dev.argon.backend.Backend
import dev.argon.compiler_tests.BackendOptionsProvider.OptionsFactory
import scala.reflect.TypeTest
import dev.argon.util.{*, given}

sealed trait BackendOptionsProvider {
  def getOptionsForBackend(backend: Backend[TestError]): Option[backend.codeGenerator.Options]
}

object BackendOptionsProvider {
  def apply(factories: OptionsFactory[?]*): BackendOptionsProvider =
    many(factories.map(forSingleFactory))

  private def empty: BackendOptionsProvider =
    new BackendOptionsProvider {
      override def getOptionsForBackend(backend: Backend[TestError]): Option[backend.codeGenerator.Options] = None
    }

  private def forSingleFactory[B <: Backend[TestError]](factory: OptionsFactory[B]): BackendOptionsProvider =
    import factory.typeTest
    new BackendOptionsProvider {
      override def getOptionsForBackend(backend: Backend[TestError]): Option[backend.codeGenerator.Options] =
        factory.typeTest.unapply(backend) match {
          case Some(b) => Some(factory.createOptions(b))
          case None => None
        }
        
    }
  end forSingleFactory

  private def many(providers: Seq[BackendOptionsProvider]): BackendOptionsProvider =
    providers match {
      case Seq() => empty
      case Seq(provider) => provider
      case _ =>
        new BackendOptionsProvider {
          override def getOptionsForBackend(backend: Backend[TestError]): Option[backend.codeGenerator.Options] =
            providers
              .view
              .flatMap { _.getOptionsForBackend(backend) }
              .headOption
        }
    }

  sealed trait OptionsFactory[B <: Backend[TestError]] {
    private[BackendOptionsProvider] val typeTest: TypeTest[Backend[TestError], B]
    private[BackendOptionsProvider] def createOptions(backend: B): backend.codeGenerator.Options
  }

  object OptionsFactory {
    def apply[B <: Backend[TestError]](f: (backend: B) => backend.codeGenerator.Options)(using tt: TypeTest[Backend[TestError], B]): OptionsFactory[B] =
      new OptionsFactory[B] {
        override val typeTest: TypeTest[Backend[TestError], B] = tt
        override def createOptions(backend: B): backend.codeGenerator.Options =
          f(backend)
      }
  }
}
