package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.NamespacePath

import scala.collection.immutable.Vector
import cats._
import cats.implicits._
import dev.argon.compiler.{Comp, Compilation}
import zio.interop.catz.core._
import zio._

object ModuleLookup {

  def lookupValues[T, TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleId)
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding[context.type, TPayloadSpec] => Comp[Option[T]])
  : Comp[Vector[T]] =
    findModule(context)(referencedModules)(moduleDesc).toList.toVector.flatTraverse { module =>
      lookupNamespaceValues(context)(module)(namespace, name)(f)
    }

  def findModule[TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleId)
  : Option[ArModule[context.type, TPayloadSpec]] =
    referencedModules.find { _.id === moduleDesc }

  def lookupNamespaceValues[T, TPayloadSpec[_, _]]
  (context: Context)
  (module: ArModule[context.type, TPayloadSpec])
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding[context.type, TPayloadSpec] => Comp[Option[T]])
  : Comp[Vector[T]] = {
    def impl(namespaceParts: Vector[String])(namespaceValues: Namespace[context.type, TPayloadSpec]): Comp[Vector[T]] =
      namespaceParts match {
        case head +: tail =>
          namespaceValues.bindings
            .filter(_.name === GlobalName.Normal(head))
            .flatTraverse {
              case GlobalBinding.NestedNamespace(_, nestedNS) => impl(tail)(nestedNS)
              case _ => IO.succeed(Vector.empty)
            }

        case Vector() =>
          namespaceValues.bindings
            .filter { _.name === name }
            .flatTraverse { binding =>
              f(binding).map { _.toList.toVector }
            }
      }

    module.globalNamespace.flatMap(impl(namespace.ns))
  }

  def lookupGlobalClass[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding[context.type, TPayloadSpec] => Comp[Option[ArClass[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalClass(_, _, Some(bindingSig), arClass) if sig === bindingSig =>
      arClass.map(Some.apply)

    case GlobalBinding.GlobalClass(_, _, None, arClassComp) =>
      arClassComp.flatMap { arClass =>
        arClass.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(arClass)
          else
            None
        }
      }

    case _ => IO.succeed(None)
  }

  def lookupGlobalTrait[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding[context.type, TPayloadSpec] => Comp[Option[ArTrait[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalTrait(_, _, Some(bindingSig), arTrait) if sig === bindingSig =>
      arTrait.map(Some.apply)

    case GlobalBinding.GlobalTrait(_, _, None, arTraitComp) =>
      arTraitComp.flatMap { arTrait =>
        arTrait.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(arTrait)
          else
            None
        }
      }
    case _ => IO.succeed(None)
  }

  def lookupGlobalDataConstructor[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding[context.type, TPayloadSpec] => Comp[Option[DataConstructor[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalDataConstructor(_, _, Some(bindingSig), ctor) if sig === bindingSig =>
      ctor.map(Some.apply)

    case GlobalBinding.GlobalDataConstructor(_, _, None, ctorComp) =>
      ctorComp.flatMap { ctor =>
        ctor.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(ctor)
          else
            None
        }
      }
    case _ => IO.succeed(None)
  }

  def lookupGlobalFunction[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature[context.type]): GlobalBinding[context.type, TPayloadSpec] => Comp[Option[ArFunc[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalFunction(_, _, Some(bindingSig), func) if sig === bindingSig =>
      func.map(Some.apply)

    case GlobalBinding.GlobalFunction(_, _, None, funcComp) =>
      funcComp.flatMap { func =>
        func.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignature(context)(sig2))
            Some(func)
          else
            None
        }
      }
    case _ => IO.succeed(None)
  }



}
