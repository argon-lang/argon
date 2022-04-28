package dev.argon.util

import zio.*
import zio.test.*
import zio.test.Assertion.*

object JavaExecuteIOTests extends DefaultRunnableSpec {

  final case class WrappedStringCause(cause: Cause[String]) extends Exception

  given ErrorWrapper[String, WrappedStringCause] with
    override def wrap(cause: Cause[String]): WrappedStringCause = WrappedStringCause(cause)
    override def unwrap(ex: WrappedStringCause): Cause[String] = ex.cause
  end given

  def runHelper(task: IO[String, Int]): IO[String, Int] =
    ZIO.runtime[Any].flatMap { runtime =>
      given runtime2: Runtime[Any] = runtime
      IO.attemptBlockingInterrupt {
        JavaExecuteIO.runInterruptable(task)
      }
        .catchAll {
          case ex: WrappedStringCause => IO.failCause(ex.cause)
          case ex => IO.die(ex)
        }
    }


  override def spec: ZSpec[Environment, Failure] =
    suite("JavaExecuteIO")(
      test("Success")(
        assertM(runHelper(IO.succeed(4)))(equalTo(4))
      ),
      test("Error")(
        assertM(runHelper(IO.fail("A")).flip)(equalTo("A"))
      ),
      test("Interrupt")(
        assertM(
          for {
            didRelease <- Ref.make(false)
            fiber <- runHelper(
              IO.bracket[Nothing, Unit, Nothing](
                acquire = IO.unit,
                release = _ => didRelease.set(true),
                use = _ => IO.never,
              )
            ).fork
            _ <- live(ZIO.sleep(1.second))
            _ <- fiber.interrupt
            res <- didRelease.get
          } yield res
        )(equalTo(true))
      ),
    )

}
