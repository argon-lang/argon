package com.mi3software.argon.util

import shapeless.{::, HList, Lazy => SLazy, Witness}
import shapeless.labelled.{FieldType, field}
import toml.Codec.{Address, Message}
import toml.{Codec, Value}

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


}
