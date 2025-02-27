package dev.argon.compiler

enum AttemptedOverload {
  case Function(f: ArFuncC)
  case Record(r: ArRecordC)
  case RecordField(r: ArRecordC, field: RecordFieldC)
}

final case class AttemptedOverloadWithErrors(
  overload: AttemptedOverload,
  errors: Seq[CompilerError],
)
