package dev.argon.util

import zio.{ZIO, IO}

object ZIOUtil {

  def swap[R, E1, E2, A](action: ZIO[R, E1, ZIO[R, E2, A]]): ZIO[R, E2, ZIO[R, E1, A]] =
    action.foldCauseM(
      failure = cause => IO.succeed(IO.halt(cause)),
      success = inner => inner.foldCauseM(
        failure = cause => IO.halt(cause),
        success = a => IO.succeed(IO.succeed(a))
      )
    )

}
