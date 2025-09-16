package dev.argon.compiler

enum AttemptedOverload {
  case Function(f: ArFuncC)
  case Record(r: ArRecordC)
  case RecordField(r: ArRecordC, field: RecordFieldC)
  case Enum(e: ArEnumC)
  case EnumVariant(variant: EnumVariantC)
  case Trait(t: ArTraitC)
}

final case class AttemptedOverloadWithErrors(
  overload: AttemptedOverload,
  errors: Seq[CompilerError],
)
