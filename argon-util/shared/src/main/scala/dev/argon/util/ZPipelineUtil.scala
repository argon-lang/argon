package dev.argon.util

import zio.*
import zio.stream.*

object ZPipelineUtil {

  def branchOnHead[R, E, A, B](f: A => ZPipeline[R, E, A, B]): ZPipeline[R, E, A, B] =
    ZPipeline.fromChannel(
      ZChannelUtil.branchOnHead[R, E, Chunk[A], Chunk[B], Any] {
        case head +: tail if tail.isEmpty =>
          f(head).toChannel

        case head +: tail =>
          (ZChannel.write(tail) *> ZChannel.identity) >>> f(head).toChannel

        case _ => branchOnHead(f).toChannel
      }
    )

}
