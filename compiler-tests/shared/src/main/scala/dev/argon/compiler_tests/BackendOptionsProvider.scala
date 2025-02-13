package dev.argon.compiler_tests

import dev.argon.backend.Backend
import dev.argon.compiler_tests.BackendOptionsProvider.OptionsFactory
import scala.reflect.TypeTest
import dev.argon.util.{*, given}

sealed trait BackendOptionsProvider {
  def getOptionsForBackend(backend: Backend): Option[backend.Options[TestError]]
}

object BackendOptionsProvider {
  def apply(factories: OptionsFactory[?]*): BackendOptionsProvider =
    many(factories.map(forSingleFactory))

  private def empty: BackendOptionsProvider =
    new BackendOptionsProvider {
      override def getOptionsForBackend(backend: Backend): Option[backend.Options[TestError]] = None
    }

  private def forSingleFactory[B <: Backend](factory: OptionsFactory[B]): BackendOptionsProvider =
    import factory.typeTest
    new BackendOptionsProvider {
      override def getOptionsForBackend(backend: Backend): Option[backend.Options[TestError]] =
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
          override def getOptionsForBackend(backend: Backend): Option[backend.Options[TestError]] =
            providers
              .view
              .flatMap { _.getOptionsForBackend(backend) }
              .headOption
        }
    }

  sealed trait OptionsFactory[B <: Backend] {
    private[BackendOptionsProvider] val typeTest: TypeTest[Backend, B]
    private[BackendOptionsProvider] def createOptions(backend: B): backend.Options[TestError]
  }

  object OptionsFactory {
    def apply[B <: Backend](f: (backend: B) => backend.Options[TestError])(using tt: TypeTest[Backend, B]): OptionsFactory[B] =
      new OptionsFactory[B] {
        override val typeTest: TypeTest[Backend, B] = tt
        override def createOptions(backend: B): backend.Options[TestError] =
          f(backend)
      }
  }
}
