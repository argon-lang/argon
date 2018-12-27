package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.Compilation
import scalaz.Scalaz._

trait TypeSystemConverter[TS1 <: TypeSystem, TS2 <: TypeSystem] {
  def convertType[A](ts1: TS1)(ts2: TS2)(t: ts1.TTypeWrapper[A]): ts2.TTypeWrapper[A]

  final def andThen[TS3 <: TypeSystem](ts2: TS2)(other: TypeSystemConverter[TS2, TS3]): TypeSystemConverter[TS1, TS3] =
    new TypeSystemConverter[TS1, TS3] {
      override def convertType[A](ts1: TS1)(ts3: TS3)(t: ts1.TTypeWrapper[A]): ts3.TTypeWrapper[A] =
        other.convertType(ts2)(ts3)(TypeSystemConverter.this.convertType(ts1)(ts2)(t))
    }
}

