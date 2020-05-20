package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.data.{NonEmptyVector, StateT}
import cats.implicits._
import zio.stream.ZStream
import zio.{Exit, IO, Queue, Ref, ZIO}

trait Source[-R, +E, +A, +X] {

  def foreach[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Unit]): ZIO[R1, E1, X]

  def foldLeftM[R1 <: R, E1 >: E, S](state: S)(f: (S, A) => ZIO[R1, E1, S]): ZIO[R1, E1, (S, X)] = for {
    stateRef <- Ref.make(state)
    result <- foreach { value =>
      for {
        state <- stateRef.get
        newState <- f(state, value)
        _ <- stateRef.set(newState)
      } yield ()
    }
    updatedState <- stateRef.get
  } yield (updatedState, result)

  def foldLeft[S](state: S)(f: (S, A) => S): ZIO[R, E, (S, X)] = for {
    stateRef <- Ref.make(state)
    result <- foreach { value =>
      stateRef.update(f(_, value))
    }
    updatedState <- stateRef.get
  } yield (updatedState, result)

  final def into[R1 <: R, E1 >: E, B, Y](f: Source[R1, E1, A, X] => Source[R1, E1, B, Y]): Source[R1, E1, B, Y] = f(this)

  def flatMap[R1 <: R, E1 >: E, B](f: A => Source[R1, E1, B, Unit]): Source[R1, E1, B, X] = new Source[R1, E1, B, X] {
    override def foreach[R2 <: R1, E2 >: E1](g: B => ZIO[R2, E2, Unit]): ZIO[R2, E2, X] =
      Source.this.foreach { a =>
        f(a).foreach(g)
      }
  }

  def map[B](f: A => B): Source[R, E, B, X] = new Source[R, E, B, X] {
    override def foreach[R1 <: R, E1 >: E](g: B => ZIO[R1, E1, Unit]): ZIO[R1, E1, X] =
      Source.this.foreach { a =>
        g(f(a))
      }
  }

  def collect[B](f: PartialFunction[A, B]): Source[R, E, B, X] = new Source[R, E, B, X] {
    override def foreach[R1 <: R, E1 >: E](g: B => ZIO[R1, E1, Unit]): ZIO[R1, E1, X] =
      Source.this.foreach { a =>
        ZIO.foreach(f.lift(a))(g).unit
      }
  }


  def mapResult[Y](f: X => Y): Source[R, E, A, Y] = new Source[R, E, A, Y] {
    override def foreach[R1 <: R, E1 >: E](g: A => ZIO[R1, E1, Unit]): ZIO[R1, E1, Y] =
      Source.this.foreach(g).map(f)
  }

  def bufferVector(count: Int): Source[R, E, NonEmptyVector[A], X] = new Source[R, E, NonEmptyVector[A], X] {
    override def foreach[R1 <: R, E1 >: E](f: NonEmptyVector[A] => ZIO[R1, E1, Unit]): ZIO[R1, E1, X] =
      Source.this.foldLeftM[R1, E1, Vector[A]](Vector.empty[A]) { (acc, value) =>
        if(acc.size + 1 >= count)
          f(NonEmptyVector.fromVectorUnsafe(acc :+ value)).as { Vector.empty[A] }
        else
          IO.succeed(acc :+ value)
      }
      .flatMap {
        case (extras, result) =>
          ZIO.foreach(NonEmptyVector.fromVector(extras))(f).as(result)
      }
  }


  def toZStream: ZStream[R, E, A] =
    ZStream.unwrap(
      for {
        queue <- Queue.bounded[Exit[Option[E], A]](1)

        _ <- foreach { value => queue.offer(Exit.Success(value)).unit }.foldCauseM(
          failure = cause => queue.offer(Exit.Failure(cause.map(Some.apply))).unit,
          success = _ => queue.offer(Exit.fail(None)).unit
        ).fork

      } yield ZStream.fromQueue(queue).collectWhileSuccess
    )


}
