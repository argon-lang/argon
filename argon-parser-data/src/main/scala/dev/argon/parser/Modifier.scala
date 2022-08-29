package dev.argon.parser

sealed trait Modifier derives CanEqual

sealed trait AccessModifier extends Modifier
sealed trait GlobalAccessModifier extends AccessModifier
sealed trait OverridabilityModifier extends Modifier
sealed trait ExtendabilityModifier extends Modifier
sealed trait ImplicitlyAbstractExtendabilityModifier extends ExtendabilityModifier
sealed trait ValueModifier extends Modifier

case object PublicModifier extends GlobalAccessModifier
case object ProtectedModifier extends AccessModifier
case object PrivateModifier extends GlobalAccessModifier
case object InternalModifier extends GlobalAccessModifier
case object VirtualModifier extends OverridabilityModifier
case object AbstractModifier extends OverridabilityModifier with ExtendabilityModifier
case object OverrideModifier extends OverridabilityModifier
case object FinalModifier extends OverridabilityModifier
case object SealedModifier extends ImplicitlyAbstractExtendabilityModifier
case object OpenModifier extends ExtendabilityModifier
case object ProofModifier extends ValueModifier
case object ErasedModifier extends ValueModifier


type ClassModifier = GlobalAccessModifier | ExtendabilityModifier
type TraitModifier = GlobalAccessModifier | ImplicitlyAbstractExtendabilityModifier
type FunctionModifier = GlobalAccessModifier | ValueModifier
type MethodModifier = AccessModifier | OverridabilityModifier | ValueModifier
type ClassConstructorModifier = AccessModifier
type LocalVariableModifier = ValueModifier

