package dev.argon.io

import zio._

object Suspend {

  private trait Producer[E, A, B] {
    def suspend(value: A): IO[E, Option[B]]
  }

  private object Producer {
    def apply[E, A, B](initialTrigger: Promise[E, Option[B]], queue: Queue[(A, Promise[E, Option[B]])]): Producer[E, A, B] = new Producer[E, A, B] {
      override def suspend(value: A): IO[E, Option[B]] = for {
        trigger <- Promise.make[E, Option[B]]
        _ <- queue.offer((value, trigger))
        result <- trigger.await
      } yield result
    }
  }

  private trait Consumer[E, A, B] {
    def next: IO[E, A]
    def done(exit: Exit[E, B]): UIO[Unit]
  }

  private object Consumer {
    def apply[E, A, B](initialTrigger: Promise[E, Option[B]], queue: Queue[(A, Promise[E, Option[B]])]): UIO[Consumer[E, A, B]] = for {
      triggerRef <- RefM.make(initialTrigger)
    } yield new Consumer[E, A, B] {
      override def next: IO[E, A] =
        triggerRef.modify { trigger =>
          trigger.succeed(None).flatMap { _ => queue.take }
        }

      override def done(exit: Exit[E, B]): UIO[Unit] =
        queue.shutdown.flatMap { _ =>
          triggerRef.get.flatMap {
            _.done(exit.map(Some.apply)).unit
          }
        }

    }

  }


  def within[R, E, A, B](producerHandler: (A => IO[E, Option[B]]) => ZIO[R, E, B], consumerHandler: IO[E, A] => ZIO[R, E, B]): ZIO[R, E, B] = for {
    queue <- Queue.bounded[(A, Promise[E, Option[B]])](1)

    initialTrigger <- Promise.make[E, Option[B]]

    consumer <- Consumer[E, A, B](initialTrigger, queue)
    consumerTask <- consumerHandler(consumer.next)
      .foldCauseM(
        failure = cause => consumer.done(Exit.Failure(cause)),
        success = b => consumer.done(Exit.Success(b))
      )
      .fork

    result <- producerHandler(Producer(initialTrigger, queue).suspend)
    _ <- queue.shutdown
    _ <- consumerTask.interrupt
  } yield result

}
