package dev.argon.parser

import zio.*
import zio.stream.*

private[parser] object StreamTextReader {

  def make[R, E](stream: ZStream[R, E, String]): ZIO[R & Scope, E, TextReader[R, E]] =
    for
      pull <- stream.toPull
    yield new TextReader[R, E] {
      override def read: ZIO[R, E, Chunk[String]] =
        pull.foldCauseZIO(
          failure = cause => sequenceCause(cause) match {
            case Some(cause) => ZIO.refailCause(cause)
            case None => ZIO.succeed(Chunk.empty)
          },
          success = chunk => ZIO.succeed(chunk)
        )
    }
    
  
  private def sequenceCause[E](cause: Cause[Option[E]]): Option[Cause[E]] =
    cause match {
      case cause: Cause.Empty.type => Some(cause)
      case cause: Cause.Die => Some(cause)
      case cause: Cause.Interrupt => Some(cause)
      case Cause.Fail(Some(e), trace) => Some(Cause.Fail(e, trace))
      case Cause.Fail(None, trace) => None
      case Cause.Stackless(cause, stackless) =>
        for
          cause <- sequenceCause(cause)
        yield Cause.Stackless(cause, stackless)
        
      case Cause.Then(left, right) =>
        for
          left <- sequenceCause(left)
          right <- sequenceCause(right)
        yield Cause.Then(left, right)
        
      case Cause.Both(left, right) =>
        for
          left <- sequenceCause(left)
          right <- sequenceCause(right)
        yield Cause.Both(left, right)
    }

}
