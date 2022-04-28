package dev.argon.util

import zio.*

private[util] final class JavaExecuteIO[R, E, EX <: Exception, A](using Runtime[R], ErrorWrapper[E, EX]) {
  private var isComplete: Boolean = false
  private var isError: Boolean = false
  private var result: A = _
  private var error: EX = _

  def execute(task: ZIO[R, E, A]): A =
    val cancel = summon[Runtime[R]].unsafeRunAsyncCancelable(task)(onComplete)

    try {
      this synchronized {
        while !isComplete do
          this.wait()
        end while
      }
    }
    catch {
      case ex: InterruptedException =>
        java.lang.System.out.println("Caught interrupt")
        cancel(FiberId.None) match {
          case Exit.Success(a) => a
          case Exit.Failure(cause) =>
            throw summon[ErrorWrapper[E, EX]].wrap(cause)
        }
    }


    java.lang.System.out.println("No interrupt")

    if isError then
      throw error
    else
      result
  end execute

  private def onComplete(exit: Exit[E, A]): Unit =
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

object JavaExecuteIO {
  def runInterruptable[R, E, EX <: Exception, A](task: ZIO[R, E, A])(using Runtime[R], ErrorWrapper[E, EX]): A =
    val exec = new JavaExecuteIO[R, E, EX, A]
    exec.execute(task)
  end runInterruptable
}
