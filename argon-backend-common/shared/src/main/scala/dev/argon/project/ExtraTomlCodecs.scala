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

  implicit def singleFileCodec: Codec[SingleFile[String]] =
    Codec { (value, defaults, index) =>
      stringCodec(value, defaults, index).map { new SingleFile(_) }
    }

  implicit def fileListCodec: Codec[FileList[String]] =
    Codec { (value, defaults, index) =>
      implicitly[Codec[List[String]]].apply(value, defaults, index).map { new FileList(_) }
    }

  implicit def fileGlobFieldCodec: Codec[FileGlob[String]] =
  Codec { (value, defaults, index) =>
    implicitly[Codec[List[String]]].apply(value, defaults, index).map { new FileGlob(_) }
  }

}
