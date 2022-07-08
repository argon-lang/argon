package dev.argon.util

import zio.*

private[util] final class JavaExecuteIO[R, E, EX <: Exception, A](using Runtime[R], ErrorWrapper[E, EX]) {
  private var isComplete: Boolean = false
  private var isError: Boolean = false
  private var result: A = _
  private var error: EX = _

  def execute(task: ZIO[R, E, A]): A =
    Unsafe.unsafe {
      val fiber = summon[Runtime[R]].unsafe.fork(task.onExit(onComplete))

      //val cancel = summon[Runtime[R]].unsafeRunAsyncCancelable(task)(onComplete)

      try {
        this synchronized {
          while !isComplete do
            this.wait()
          end while
        }
      }
      catch {
        case ex: InterruptedException =>
          val exit = summon[Runtime[R]].unsafe
            .run(fiber.interrupt)
            .flattenExit

          exit match {
            case Exit.Success(a) => return a
            case Exit.Failure(cause) =>
              throw summon[ErrorWrapper[E, EX]].wrap(cause)
          }
      }

      if isError then
        throw error
      else
        result
    }

  private def onComplete(exit: Exit[E, A]): UIO[Unit] =
    ZIO.succeed {
      exit match {
        case Exit.Success(a) =>
          this synchronized {
            isComplete = true
            result = a
          }

        case Exit.Failure(cause) =>
          this synchronized {
            isComplete = true
            isError = true
            error = summon[ErrorWrapper[E, EX]].wrap(cause)
          }
      }
    }
}

object JavaExecuteIO {
  def runInterruptable[R, E, EX <: Exception, A](task: ZIO[R, E, A])(using Runtime[R], ErrorWrapper[E, EX]): A =
    val exec = new JavaExecuteIO[R, E, EX, A]
    exec.execute(task)
  end runInterruptable
}
