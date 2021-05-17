package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.NamespacePath

import scala.collection.immutable.Vector
import cats.implicits._
import dev.argon.compiler.{Comp, CompStream}
import zio._
import zio.stream.Stream

object ModuleLookup {

  def lookupValues[T, TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleId)
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[T]])
  : CompStream[T] =
    Stream(findModule(context)(referencedModules)(moduleDesc))
      .collectSome
      .flatMap { module =>
        module.lookupNamespaceBindings(namespace, name)(f)
      }

  def findModule[TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleId)
  : Option[ArModule[context.type, TPayloadSpec]] =
    referencedModules.find { _.id === moduleDesc }

  def lookupGlobalClass[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[ArClass[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalClass(_, _, Some(bindingSig), arClass) if sig === bindingSig =>
      arClass.asSome

    case GlobalBinding.GlobalClass(_, _, None, arClassComp) =>
      arClassComp.flatMap { arClass =>
        arClass.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(arClass)
          else
            None
        }
      }

    case _ => IO.none
  }

  def lookupGlobalTrait[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[ArTrait[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalTrait(_, _, Some(bindingSig), arTrait) if sig === bindingSig =>
      arTrait.asSome

    case GlobalBinding.GlobalTrait(_, _, None, arTraitComp) =>
      arTraitComp.flatMap { arTrait =>
        arTrait.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(arTrait)
          else
            None
        }
      }
    case _ => IO.none
  }

  def lookupGlobalDataConstructor[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature.ParameterOnlySignature[context.type]): GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[DataConstructor[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalDataConstructor(_, _, Some(bindingSig), ctor) if sig === bindingSig =>
      ctor.asSome

    case GlobalBinding.GlobalDataConstructor(_, _, None, ctorComp) =>
      ctorComp.flatMap { ctor =>
        ctor.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignatureParameters(context)(sig2))
            Some(ctor)
          else
            None
        }
      }
    case _ => IO.none
  }

  def lookupGlobalFunction[TPayloadSpec[_, _]](context: Context)(sig: ErasedSignature[context.type]): GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[ArFunc[context.type, TPayloadSpec]]] = {
    case GlobalBinding.GlobalFunction(_, _, Some(bindingSig), func) if sig === bindingSig =>
      func.asSome

    case GlobalBinding.GlobalFunction(_, _, None, funcComp) =>
      funcComp.flatMap { func =>
        func.signature.map { sig2 =>
          if(sig === ErasedSignature.fromSignature(context)(sig2))
            Some(func)
          else
            None
        }
      }
    case _ => IO.none
  }



}
