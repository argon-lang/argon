package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.{Definition, TypeParameter}

object GeneratorUtil {
  def getDefinitionName(definition: Definition): String =
    definition match
      case Definition.ConstructorDef(constructor) => constructor.name
      case Definition.Enum(name, _*) => name
      case Definition.Const(name, _, _) => name
      case Definition.SimpleEnum(name, _*) => name
      case Definition.EnumClass(name, _, _*) => name
      case Definition.Interface(name, _, _, _*) => name
      case Definition.TypeStruct(name, _*) => name
      case Definition.Extern(name, _) => name
    end match

  def getDefinitionTypeParameters(definition: Definition): Seq[TypeParameter] =
    definition match
      case Definition.ConstructorDef(_) => Seq()
      case Definition.Enum(_, _*) => Seq()
      case Definition.Const(_, _, _) => Seq()
      case Definition.SimpleEnum(_, _*) => Seq()
      case Definition.EnumClass(_, typeParameters, _*) => typeParameters
      case Definition.Interface(_, typeParameters, _, _*) => typeParameters
      case Definition.TypeStruct(_, _*) => Seq()
      case Definition.Extern(_, typeParameters) => typeParameters
    end match
    
  def isReferenceType(definition: Definition): Boolean =
    definition match
      case Definition.EnumClass(_, _, _*) | Definition.Interface(_, _, _, _*) | Definition.Extern(_, _) => true
      case _ => false
    end match
    
}
