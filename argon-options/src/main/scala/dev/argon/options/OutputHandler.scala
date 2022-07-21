package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.options

import scala.deriving.Mirror

trait OutputHandler[R, E, Output]:
  lazy val options: Seq[OutputInfo[R, E, Output]]
end OutputHandler

object OutputHandler:

  final class OutputHandlerDerivation[R, E] extends ProductDerivation[[A] =>> OutputHandler[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OutputHandler[R, E, T] =
      new OutputHandler[R, E, T] {
        override lazy val options: Seq[OutputInfo[R, E, T]] =
          ctx.params.flatMap { param =>
            param.typeclass.options.map { subOpt =>
              new OutputInfo[R, E, T]:

                override val name: Seq[String] =
                  param.label +: subOpt.name

                override def getValue(options: T): BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource] =
                  subOpt.getValue(param.deref(options))
              end new
            }
          }
      }

  }

  inline def derive[R, E, A](using Mirror.Of[A]): OutputHandler[R, E, A] =
    new OutputHandlerDerivation[R, E].derivedMirror[A]


  given binaryResourceOutputHandler[R, E, Res <: BinaryResource[R, E]]: OutputHandler[R, E, Res] with
    override lazy val options: Seq[OutputInfo[R, E, Res]] =
      Seq(new OutputInfo[R, E, Res] {
        override val name: Seq[String] = Seq()
        override def getValue(options: Res): BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource] =
          options
      })
  end binaryResourceOutputHandler

  given directoryResourceOutputHandler[R, E, FileRes[-R2, +E2] <: BinaryResource[R2, E2], Res <: DirectoryResource[R, E, FileRes]]: OutputHandler[R, E, Res] with
    override lazy val options: Seq[OutputInfo[R, E, Res]] =
      Seq(new OutputInfo[R, E, Res] {
        override val name: Seq[String] = Seq()
        override def getValue(options: Res): BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource] =
          options
      })
  end directoryResourceOutputHandler

end OutputHandler


