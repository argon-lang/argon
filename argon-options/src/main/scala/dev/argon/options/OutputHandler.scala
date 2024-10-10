package dev.argon.options

import dev.argon.io.*
import dev.argon.options

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait OutputHandler[+E, -Output]:
  lazy val options: Map[Seq[String], OutputInfo[E, Output]]
end OutputHandler

object OutputHandler:

  inline def derive[E, A <: Product](using m: Mirror.ProductOf[A]): OutputHandler[E, A] =
    lazy val opts = getFieldInfo[E, m.MirroredElemLabels, m.MirroredElemTypes]
    ProductOutputHandler(opts)
  end derive

  inline def getFieldInfo[E, Labels <: Tuple, Elems <: Tuple]: Map[Seq[String], OutputInfo[E, Elems]] =
    inline erasedValue[(Labels, Elems)] match {
      case _: (hlabel *: tlabels, helem *: telems) =>
        val label = constValue[hlabel & String]
        val fieldHandler = summonInline[OutputHandler[E, helem]]
        val tailFieldInfo = getFieldInfo[E, tlabels, telems]

        val fieldInfo = fieldHandler.options.map { (key, info) =>
          (label +: key) -> HeadFieldOutputInfo[E, helem, telems](info)
        }

        val tailFieldInfo2 = tailFieldInfo.view.mapValues { info =>
          TailFieldOutputInfo[E, helem, telems](info)
        }

        val fields = fieldInfo ++ tailFieldInfo2

        summonInline[Map[Seq[String], OutputInfo[E, helem *: telems]] <:< Map[Seq[String], OutputInfo[E, Elems]]](fields)

      case _: (EmptyTuple, EmptyTuple) =>
        Map.empty
    }

  final class ProductOutputHandler[E, A <: Product](using m: Mirror.ProductOf[A])(opts: Map[Seq[String], OutputInfo[E, m.MirroredElemTypes]]) extends OutputHandler[E, A] {
    override lazy val options: Map[Seq[String], OutputInfo[E, A]] =
      opts.view.mapValues { info =>
        new OutputInfo[E, A] {
          override def getValue(options: A): FileSystemResource[E, BinaryResource] =
            info.getValue(Tuple.fromProductTyped(options))
        }
      }.toMap
  }
  

  final class ProductOutputInfo[E, A <: Product](using m: Mirror.ProductOf[A])(info: OutputInfo[E, m.MirroredElemTypes]) extends OutputInfo[E, A] {
    override def getValue(options: A): FileSystemResource[E, BinaryResource] =
      info.getValue(Tuple.fromProductTyped(options))
  }

  final class HeadFieldOutputInfo[E, H, T <: Tuple](info: OutputInfo[E, H]) extends OutputInfo[E, H *: T] {
    override def getValue(options: H *: T): FileSystemResource[E, BinaryResource] =
      val (h *: _) = options
      info.getValue(h)
    end getValue
  }

  final class TailFieldOutputInfo[E, H, T <: Tuple](info: OutputInfo[E, T]) extends OutputInfo[E, H *: T] {
    override def getValue(options: H *: T): FileSystemResource[E, BinaryResource] =
      val (_ *: t) = options
      info.getValue(t)
    end getValue
  }



  given binaryResourceOutputHandler[E, Res <: BinaryResource[E]]: OutputHandler[E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[E, Res]] =
      Map(Seq() -> new OutputInfo[E, Res] {
        override def getValue(options: Res): FileSystemResource[E, BinaryResource] =
          FileSystemResource.Of(options)
      })
  end binaryResourceOutputHandler

  given directoryResourceOutputHandler[E, FileRes[+E2] <: BinaryResource[E2], Res <: DirectoryResource[E, FileRes]]: OutputHandler[E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[E, Res]] =
      Map(Seq() -> new OutputInfo[E, Res] {
        override def getValue(options: Res): FileSystemResource[E, BinaryResource] =
          options
      })
  end directoryResourceOutputHandler

  given OutputHandler[Nothing, Nothing] with
    override lazy val options: Map[Seq[String], OutputInfo[Nothing, Nothing]] = Map.empty
  end given

end OutputHandler


