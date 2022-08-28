package dev.argon.parser

sealed trait Modifier derives CanEqual

sealed trait AccessModifier extends Modifier
sealed trait GlobalAccessModifier extends AccessModifier
sealed trait OverridabilityModifier extends Modifier
sealed trait ExtendabilityModifier extends Modifier


case object PublicModifier extends GlobalAccessModifier
case object ProtectedModifier extends AccessModifier
case object PrivateModifier extends GlobalAccessModifier
case object InternalModifier extends GlobalAccessModifier
case object VirtualModifier extends OverridabilityModifier
case object AbstractModifier extends OverridabilityModifier with ExtendabilityModifier
case object OverrideModifier extends OverridabilityModifier
case object FinalModifier extends OverridabilityModifier
case object SealedModifier extends ExtendabilityModifier
case object OpenModifier extends ExtendabilityModifier
case object ProofModifier extends Modifier


type ClassModifier = GlobalAccessModifier | ExtendabilityModifier
type TraitModifier = GlobalAccessModifier | SealedModifier.type
type FunctionModifier = GlobalAccessModifier | ProofModifier.type
type MethodModifier = AccessModifier | OverridabilityModifier | ProofModifier.type
type ClassConstructorModifier = AccessModifier
type LocalVariableModifier = ProofModifier.type

