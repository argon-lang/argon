package dev.argon.vm

import dev.argon.util.*

opaque type FunctionId = UniqueIdentifier
opaque type LocalId = UniqueIdentifier
opaque type ClassId = UniqueIdentifier
opaque type FieldId = UniqueIdentifier

final case class LabelId(id: UniqueIdentifier)

