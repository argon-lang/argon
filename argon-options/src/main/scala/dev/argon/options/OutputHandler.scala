package dev.argon.options

import dev.argon.io.*
import dev.argon.options

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait OutputHandler[-R, +E, Output]:
  lazy val options: Map[Seq[String], OutputInfo[R, E, Output]]
end OutputHandler

object OutputHandler:

  inline def derive[R, E, A <: Product](using m: Mirror.ProductOf[A]): OutputHandler[R, E, A] =
    lazy val opts = getFieldInfo[R, E, m.MirroredElemLabels, m.MirroredElemTypes]
    new OutputHandler[R, E, A] {
      override lazy val options: Map[Seq[String], OutputInfo[R, E, A]] =
        opts.view.mapValues { info =>
          new OutputInfo[R, E, A] {
            override def getValue(options: A): FileSystemResource[R, E, BinaryResource] =
              info.getValue(Tuple.fromProductTyped(options))
          }
        }.toMap
    }
  end derive

  inline def getFieldInfo[R, E, Labels <: Tuple, Elems <: Tuple]: Map[Seq[String], OutputInfo[R, E, Elems]] =
    inline erasedValue[(Labels, Elems)] match {
      case _: (hlabel *: tlabels, helem *: telems) =>
        val label = constValue[hlabel & String]
        val fieldHandler = summonInline[OutputHandler[R, E, helem]]
        val tailFieldInfo = getFieldInfo[R, E, tlabels, telems]

        val fieldInfo = fieldHandler.options.map { (key, info) =>
          (label +: key) -> new OutputInfo[R, E, helem *: telems] {
            override def getValue(options: helem *: telems): FileSystemResource[R, E, BinaryResource] =
              val (h *: _) = options
              info.getValue(h)
            end getValue
          }
        }

        val tailFieldInfo2 = tailFieldInfo.view.mapValues { info =>
          new OutputInfo[R, E, helem *: telems] {
            override def getValue(options: helem *: telems): FileSystemResource[R, E, BinaryResource] =
              val (_ *: t) = options
              info.getValue(t)
            end getValue
          }
        }

        val fields = fieldInfo ++ tailFieldInfo2

        summonInline[Map[Seq[String], OutputInfo[R, E, helem *: telems]] <:< Map[Seq[String], OutputInfo[R, E, Elems]]](fields)

      case _: (EmptyTuple, EmptyTuple) =>
        Map.empty
    }

  given binaryResourceOutputHandler[R, E, Res <: BinaryResource[R, E]]: OutputHandler[R, E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[R, E, Res]] =
      Map(Seq() -> new OutputInfo[R, E, Res] {
        override def getValue(options: Res): FileSystemResource[R, E, BinaryResource] =
          FileSystemResource.Of(options)
      })
  end binaryResourceOutputHandler

  given directoryResourceOutputHandler[R, E, FileRes[-R2, +E2] <: BinaryResource[R2, E2], Res <: DirectoryResource[R, E, FileRes]]: OutputHandler[R, E, Res] with
    override lazy val options: Map[Seq[String], OutputInfo[R, E, Res]] =
      Map(Seq() -> new OutputInfo[R, E, Res] {
        override def getValue(options: Res): FileSystemResource[R, E, BinaryResource] =
          options
      })
  end directoryResourceOutputHandler

  given OutputHandler[Any, Nothing, Nothing] with
    override lazy val options: Map[Seq[String], OutputInfo[Any, Nothing, Nothing]] = Map.empty
  end given

end OutputHandler


