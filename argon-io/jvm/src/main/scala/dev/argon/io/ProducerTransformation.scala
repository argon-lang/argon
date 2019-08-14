package dev.argon.io

import cats.data.NonEmptyVector
import dev.argon.stream.{Resource, Step, StreamTransformation}
import zio._

private[io] abstract class ProducerTransformation[R, E, A, B] extends StreamTransformation[ZIO, R, E, A, Unit, Nothing, B] {
  override type State = Producer

  trait Producer {
    private[ProducerTransformation] def suspend(value: Vector[A]): IO[E, Option[B]]
    private[ProducerTransformation] val initialValue: Option[B]
  }

  private trait Consumer {
    def next: IO[E, Vector[A]]
    def done(exit: Exit[E, B]): UIO[Unit]
  }

  protected def consumerHandler(getNext: IO[E, Vector[A]]): ZIO[R, E, B]

  override def initial: Resource[ZIO, R, E, State] =
    Resource.fromZManaged(
      ZManaged.fromEffect(
        for {
          initialTrigger <- Promise.make[E, Option[B]]
          queue <- Queue.bounded[(Vector[A], Promise[E, Option[B]])](1)
          consumer <- for {
            triggerRef <- RefM.make(initialTrigger)
          } yield new Consumer {
            override def next: IO[E, Vector[A]] =
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

          _ <- consumerHandler(consumer.next)
            .foldCauseM(
              failure = cause => consumer.done(Exit.Failure(cause)),
              success = b => consumer.done(Exit.Success(b))
            )
            .fork

          initValue <- initialTrigger.await


        } yield new Producer {
          private[ProducerTransformation] override def suspend(value: Vector[A]): IO[E, Option[B]] = for {
            trigger <- Promise.make[E, Option[B]]
            _ <- queue.offer((value, trigger))
            result <- trigger.await
          } yield result

          override private[ProducerTransformation] val initialValue = initValue
        }
      )
    )

  override def step(s: Producer, ca: NonEmptyVector[A]): ZIO[R, E, Step[Producer, A, Nothing, B]] =
    s.initialValue match {
      case None =>
        s.suspend(ca.toVector).map {
          case Some(b) => Step.Stop(b)
          case None => Step.Continue(s)
        }

      case Some(b) => IO.succeed(Step.Stop(b))
    }

  private def sendEOF(producer: Producer): ZIO[R, E, B] =
    producer.suspend(Vector.empty).flatMap {
      case Some(b) => IO.succeed(b)
      case None => sendEOF(producer)
    }

  override def end(s: Producer, result: Unit): ZIO[R, E, (Vector[Nothing], ZIO[R, E, B])] =
    IO.succeed((Vector.empty, sendEOF(s)))
}
