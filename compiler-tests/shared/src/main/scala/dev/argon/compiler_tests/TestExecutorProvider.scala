package dev.argon.compiler_tests

import dev.argon.backend.Backend
import dev.argon.compiler_tests.TestExecutorProvider.ExecutorFactory
import scala.reflect.TypeTest
import dev.argon.util.{*, given}

sealed trait TestExecutorProvider {
  def getExecutorForBackend(backend: Backend): Option[TestExecutor.Aux[backend.type]]
}

object TestExecutorProvider {
  def apply(factories: ExecutorFactory[?]*): TestExecutorProvider =
    many(factories.map(forSingleFactory))

  private def empty: TestExecutorProvider =
    new TestExecutorProvider {
      override def getExecutorForBackend(backend: Backend): Option[TestExecutor.Aux[backend.type]] = None
    }

  private def forSingleFactory[B <: Backend](factory: ExecutorFactory[B]): TestExecutorProvider =
    import factory.typeTest
    new TestExecutorProvider {
      override def getExecutorForBackend(backend: Backend): Option[TestExecutor.Aux[backend.type]] =
        factory.typeTest.unapply(backend) match {
          case Some(b) => Some(factory.createExecutor(b))
          case None => None
        }
        
    }
  end forSingleFactory

  private def many(providers: Seq[TestExecutorProvider]): TestExecutorProvider =
    providers match {
      case Seq() => empty
      case Seq(provider) => provider
      case _ =>
        new TestExecutorProvider {
          override def getExecutorForBackend(backend: Backend): Option[TestExecutor.Aux[backend.type]] =
            providers
              .view
              .flatMap { _.getExecutorForBackend(backend) }
              .headOption
        }
    }

  sealed trait ExecutorFactory[B <: Backend] {
    private[TestExecutorProvider] val typeTest: TypeTest[Backend, B]
    private[TestExecutorProvider] def createExecutor(backend: B): TestExecutor.Aux[backend.type]
  }

  object ExecutorFactory {
    def apply[B <: Backend](f: (backend: B) => TestExecutor.Aux[backend.type])(using tt: TypeTest[Backend, B]): ExecutorFactory[B] =
      new ExecutorFactory[B] {
        override val typeTest: TypeTest[Backend, B] = tt
        override def createExecutor(backend: B): TestExecutor.Aux[backend.type] =
          f(backend)
      }
  }
}
