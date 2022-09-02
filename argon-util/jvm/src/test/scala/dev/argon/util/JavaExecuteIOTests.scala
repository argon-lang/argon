package dev.argon.util

import zio.*
import zio.test.*
import zio.test.Assertion.*

object JavaExecuteIOTests extends ZIOSpecDefault {

  final case class WrappedStringCause(cause: Cause[String]) extends Exception

  given ErrorWrapper[String, WrappedStringCause] with
    override def wrap(cause: Cause[String]): WrappedStringCause = WrappedStringCause(cause)
    override def unwrap(ex: WrappedStringCause): Cause[String] = ex.cause
  end given

  def runHelper(task: IO[String, Int]): IO[String, Int] =
    ZIO.runtime[Any].flatMap { runtime =>
      given runtime2: Runtime[Any] = runtime
      ZIO.attemptBlockingInterrupt {
        JavaExecuteIO.runInterruptable(task)
      }
        .catchAll {
          case ex: WrappedStringCause => ZIO.refailCause(ex.cause)
          case ex => ZIO.die(ex)
        }
    }


  override def spec: Spec[Environment & Scope, Any] =
    suite("JavaExecuteIO")(
      test("Success")(
        assertZIO(runHelper(ZIO.succeed(4)))(equalTo(4))
      ),
      test("Error")(
        assertZIO(runHelper(ZIO.fail("A")).flip)(equalTo("A"))
      ),
      test("Interrupt")(
        assertZIO(
          for {
            didRelease <- Ref.make(false)
            fiber <- runHelper(
              ZIO.acquireReleaseWith(acquire = ZIO.unit)(release = _ => didRelease.set(true))(use = _ => ZIO.never)
            ).fork
            _ <- live(ZIO.sleep(1.second))
            _ <- fiber.interrupt
            res <- didRelease.get
          } yield res
        )(equalTo(true))
      ),
    )

}
