package dev.argon.plugin.scalaApi

sealed trait ExternRefKind
sealed trait ExternKind
trait ExternRefKindFunction extends ExternRefKind
trait ExternKindFunction extends ExternKind with ExternRefKindFunction
trait ExternRefKindRecord extends ExternRefKind


type ExternFunctionImpl[Externs] = Externs & ExternKindFunction
type ExternFunctionRef[Externs] = Externs & ExternRefKindFunction

type ExternRecordRef[Externs] = Externs & ExternRefKindRecord

