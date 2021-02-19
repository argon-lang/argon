package dev.argon.compiler.output

import java.nio.charset.StandardCharsets

import dev.argon.compiler.output.TextBuildArtifact.stringBuilderSink
import dev.argon.compiler.{Comp, CompStream}
import zio.stream._
import zio.{Chunk, IO}

trait TextBuildArtifact extends BuildArtifact {
  def asStringStream: CompStream[String]

  def asString: Comp[String] = asStringStream.run(stringBuilderSink)

  override def asStream: CompStream[Byte] =
    asStringStream.mapChunks { strs =>
      strs.flatMap { str =>
        Chunk.fromArray(str.getBytes(StandardCharsets.UTF_8))
      }
    }
}

object TextBuildArtifact {


  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def stringBuilderSink: Sink[Nothing, String, Nothing, String] =
    ZSink.fromEffect(IO.effectTotal { new StringBuilder }).flatMap { sb =>
      ZSink.fromPush {
        case Some(value) => IO.effectTotal { value.foreach(sb.append) }
        case None => IO.fail((Right(sb.toString()), Chunk.empty))
      }
    }
}
