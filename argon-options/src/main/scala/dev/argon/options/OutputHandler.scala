package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.options

import scala.deriving.Mirror

trait OutputHandler[R, E, Output]:
  lazy val options: Map[Seq[String], OutputInfo[R, E, Output]]
end OutputHandler

object OutputHandler:

  final class OutputHandlerDerivation[R, E] extends ProductDerivation[[A] =>> OutputHandler[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OutputHandler[R, E, T] =
      new OutputHandler[R, E, T] {
        override lazy val options: Map[Seq[String], OutputInfo[R, E, T]] =
          ctx.params
            .iterator
            .flatMap { param =>
              param.typeclass.options
                .iterator
                .map { case (subOptName, subOpt) =>
                  val info =
                    new OutputInfo[R, E, T]:
                      override def getValue(options: T): FileSystemResource[R, E] =
                        subOpt.getValue(param.deref(options))
                    end new

                  (param.label +: subOptName) -> info
                }
            }
            .toMap
      }

  }

  inline def derive[R, E, A](using Mirror.Of[A]): OutputHandler[R, E, A] =
    new OutputHandlerDerivation[R, E].derivedMirror[A]


  given binaryResourceOutputHandler[R, E, Res <: BinaryResource[R, E]]: OutputHandler[R, E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[R, E, Res]] =
      Map(Seq() -> new OutputInfo[R, E, Res] {
        override def getValue(options: Res): FileSystemResource[R, E] =
          options
      })
  end binaryResourceOutputHandler

  given directoryResourceOutputHandler[R, E, FileRes[-R2, +E2] <: BinaryResource[R2, E2], Res <: DirectoryResource[R, E, FileRes]]: OutputHandler[R, E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[R, E, Res]] =
      Map(Seq() -> new OutputInfo[R, E, Res] {
        override def getValue(options: Res): FileSystemResource[R, E] =
          options
      })
  end directoryResourceOutputHandler

end OutputHandler


