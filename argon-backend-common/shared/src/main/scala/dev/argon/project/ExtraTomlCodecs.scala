package dev.argon.project

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, Witness, Lazy => SLazy}
import toml.Codec
import toml.Codecs._

object ExtraTomlCodecs {

  implicit def nestedOptionFieldCodec[K <: Symbol, V, T <: HList]
  (implicit
    witness: Witness.Aux[K],
    fromV: SLazy[Codec[V]],
    fromT: SLazy[Codec[T]]
  ): Codec[FieldType[K, Option[Option[V]]] :: T] =
    Codec { (value, defaults, index) =>
      toml.Codecs.hconsFromNodeOpt[K, V, T].apply(value, defaults, index)
        .map { case head :: tail =>
          val newHead = head.map(Some.apply : V => Option[V])
          field[K](newHead) :: tail
        }
    }

  implicit def singleFileFieldCodec[K <: Symbol, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   fromT: SLazy[Codec[T]]
  ): Codec[FieldType[K, SingleFile[String]] :: T] =
    Codec { (value, defaults, index) =>
      toml.Codecs.hconsFromNode[K, String, T].apply(value, defaults, index)
        .map { case head :: tail =>
          val newHead = SingleFile[String](head)
          field[K](newHead) :: tail
        }
    }

  implicit def fileListFieldCodec[K <: Symbol, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   fromT: SLazy[Codec[T]]
  ): Codec[FieldType[K, FileList[String]] :: T] =
    Codec { (value, defaults, index) =>
      toml.Codecs.hconsFromNode[K, List[String], T].apply(value, defaults, index)
        .map { case head :: tail =>
          val newHead = FileList(head)
          field[K](newHead) :: tail
        }
    }

  implicit def fileGlobFieldCodec[K <: Symbol, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   fromT: SLazy[Codec[T]]
  ): Codec[FieldType[K, FileGlob[String]] :: T] =
    Codec { (value, defaults, index) =>
      toml.Codecs.hconsFromNode[K, List[String], T].apply(value, defaults, index)
        .map { case head :: tail =>
          val newHead = FileGlob(head)
          field[K](newHead) :: tail
        }
    }

}
