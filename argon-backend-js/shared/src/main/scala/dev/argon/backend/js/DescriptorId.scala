package dev.argon.backend.js

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.NamespacePath
import cats._
import cats.implicits._

object DescriptorId {

  private val specialChars = Vector("\\", ".", ",", ":", "-", "@", "#", "$", "?", "(", ")", "[", "]", "{", "}", "<", ">")

  private def encodeIdentifier(id: String): String =
    specialChars.foldLeft(id) { (id, ch) => id.replace(ch, "\\" + ch) }

  private def encodeInNamespace(descriptor: InNamespaceDescriptor): String =
    descriptor.namespace.ns.map(encodeIdentifier).map(_ + ".").mkString +
      ((descriptor.name match {
        case GlobalName.Normal(id) => encodeIdentifier(id)
        case GlobalName.Unnamed => "#" + descriptor.id.toString
      }) : String)

  private def encodeType[TContext <: Context with Singleton](t: ErasedSignature.SigType[TContext]): String =
    t match {
      case ErasedSignature.BlankType() => "?"
      case ErasedSignature.TraitType(arTrait) =>
        arTrait.value.descriptor match {
          case desc @ TraitDescriptor.InNamespace(_, _, _, _) => s"(${encodeInNamespace(desc)})"
        }

      case ErasedSignature.ClassType(arClass) =>
        arClass.value.descriptor match {
          case desc @ ClassDescriptor.InNamespace(_, _, _, _) => s"(${encodeInNamespace(desc)})"
        }

      case ErasedSignature.DataConstructorType(ctor) =>
        ctor.value.descriptor match {
          case desc @ DataConstructorDescriptor.InNamespace(_, _, _, _) => s"(${encodeInNamespace(desc)})"
        }

      case ErasedSignature.FunctionType(argumentType, resultType) => s"(${encodeType(argumentType)}->${encodeType(resultType)})"
      case ErasedSignature.TupleType(elements) => s"(${elements.map(e => "$_:" + encodeType(e)).toList.toVector.mkString(",")})"
    }

  private def encodeSignature[TContext <: Context with Singleton](signature: ErasedSignature[TContext]): String =
    signature match {
      case ErasedSignature.Parameter(paramType, next) =>
        encodeType(paramType) + "->" + encodeSignature(next)

      case ErasedSignature.Result(resultType) =>
        encodeType(resultType)
    }

  private def encodeSignatureParameters[TContext <: Context with Singleton](signature: ErasedSignature.ParameterOnlySignature[TContext]): String =
    signature.paramTypes.map { paramType => encodeType(paramType) + "->" }.mkString

  private def encodeMethodName[TContext <: Context with Singleton](methodName: MethodName): String =
    methodName match {
      case MemberName.Normal(name) => encodeIdentifier(name)
      case MemberName.Unnamed => "$_"
      case MemberName.Call => "$call"
    }

  def forClass[TContext <: Context with Singleton](descriptor: ClassDescriptor, signature: ErasedSignature.ParameterOnlySignature[TContext]): String =
    descriptor match {
      case descriptor @ ClassDescriptor.InNamespace(_, _, _, _) => s"${encodeInNamespace(descriptor)}:${encodeSignatureParameters(signature)}"
    }

  def forTrait[TContext <: Context with Singleton](descriptor: TraitDescriptor, signature: ErasedSignature.ParameterOnlySignature[TContext]): String =
    descriptor match {
      case descriptor @ TraitDescriptor.InNamespace(_, _, _, _) => s"${encodeInNamespace(descriptor)}:${encodeSignatureParameters(signature)}"
    }

  def forDataConstructor[TContext <: Context with Singleton](descriptor: DataConstructorDescriptor, signature: ErasedSignature.ParameterOnlySignature[TContext]): String =
    descriptor match {
      case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _) => s"${encodeInNamespace(descriptor)}:${encodeSignatureParameters(signature)}"
    }

  def forFunc[TContext <: Context with Singleton](descriptor: FuncDescriptor, signature: ErasedSignature[TContext]): String =
    descriptor match {
      case descriptor @ FuncDescriptor.InNamespace(_, _, _, _) => s"${encodeInNamespace(descriptor)}:${encodeSignature(signature)}"
    }

  def forMethod[TContext <: Context with Singleton](descriptor: MethodDescriptor, signature: ErasedSignature[TContext]): String =
    s"${encodeMethodName(descriptor.name)}:${encodeSignature(signature)}"

  def forClassConstructor[TContext <: Context with Singleton](signature: ErasedSignature.ParameterOnlySignature[TContext]): String =
    encodeSignatureParameters(signature)

}
