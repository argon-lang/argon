package com.mi3software.argon.compiler.module

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import com.mi3software.argon.util.stream.{ArStream, StreamTransformation}
import scalapb.GeneratedMessage
import scalaz.{ Lens => _, _ }
import Scalaz._
import com.mi3software.argon.compiler.types.TypeSystem
import com.mi3software.argon.util.NonEmptyVector
import com.mi3software.argon.{module2 => module}
import shapeless._

final class ModuleEmitter[TComp[+_] : Compilation, TContext <: ModuleContext[TComp] with Singleton](context: TContext) {

  import context.signatureContext.Signature
  import context.typeSystem

  private final case class ModuleIds
  (
    nextClassId: Int = 1,
    nextTraitId: Int = 1,
    nextDataCtorId: Int = 1,
    nextFunctionId: Int = 1,
    nextMethodId: Int = 1,
    nextClassCtorId: Int = 1,

    classIds: Map[ClassDescriptor, Int] = Map(),
    traitIds: Map[TraitDescriptor, Int] = Map(),
    dataCtorIds: Map[DataConstructorDescriptor, Int] = Map(),
    functionIds: Map[FuncDescriptor, Int] = Map(),
    methodIds: Map[MethodDescriptor, Int] = Map(),
    classCtorIds: Map[ClassConstructorDescriptor, Int] = Map(),

    nextClassRefId: Int = 1,
    nextTraitRefId: Int = 1,
    nextDataCtorRefId: Int = 1,
    nextFunctionRefId: Int = 1,
    nextMethodRefId: Int = 1,
    nextClassCtorRefId: Int = 1,

    classRefIds: Map[ClassDescriptor, Int] = Map(),
    traitRefIds: Map[TraitDescriptor, Int] = Map(),
    dataCtorRefIds: Map[DataConstructorDescriptor, Int] = Map(),
    functionRefIds: Map[FuncDescriptor, Int] = Map(),
    methodRefIds: Map[MethodDescriptor, Int] = Map(),
    classCtorRefIds: Map[ClassConstructorDescriptor, Int] = Map(),
  )

  private type Emit[A] = StateT[TComp, ModuleIds, A]

  private def emitComp[A](comp: TComp[A]): Emit[A] = StateT.liftM(comp)


  private def globalTransform[F[_]](fromComp: TComp ~> F): StreamTransformation[F, Namespace[context.type, DeclarationPayloadSpecifier], Unit, (String, GeneratedMessage), Unit] =
    new StreamTransformation.Single[F, Namespace[context.type, DeclarationPayloadSpecifier], Unit, (String, GeneratedMessage), Unit] {
      override type S = ModuleIds
      override val initialState: ModuleIds = ModuleIds()

      private def processMethods[S2](state: ModuleIds, state2: S2, methods: Vector[ArMethod[context.type, DeclarationPayloadSpecifier]])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(ModuleIds, S2)] =
        methods.foldLeftM((state, state2)) { case ((state, state2), method) =>
          fromComp(createMethodDefMessage(method).run(state))
            .flatMap { case (state, result) =>
              f(state2, NonEmptyVector.of(result)).map { (state, _) }
            }
        }

      private def processClassCtors[S2](state: ModuleIds, state2: S2, ctors: Vector[ClassConstructor[context.type, DeclarationPayloadSpecifier]])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(ModuleIds, S2)] =
        ctors.foldLeftM((state, state2)) { case ((state, state2), ctor) =>
          fromComp(createClassCtorDefMessage(ctor).run(state))
            .flatMap { case (state, result) =>
              f(state2, NonEmptyVector.of(result)).map { (state, _) }
            }
        }

      private def processBinding[S2](state: ModuleIds, state2: S2, binding: GlobalBinding[context.type, DeclarationPayloadSpecifier])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(ModuleIds, S2)] =
        binding match {
          case GlobalBinding.GlobalTrait(_, _, arTrait) =>
            for {
              (state, result) <- fromComp(createTraitDefMessage(arTrait).run(state))
              state2 <- f(state2, NonEmptyVector.of(result))

              instMethods <- fromComp(arTrait.methods)
              (state, state2) <- processMethods(state, state2, instMethods)(f)

              staticMethods <- fromComp(arTrait.staticMethods)
              (state, state2) <- processMethods(state, state2, staticMethods)(f)

            } yield (state, state2)

          case GlobalBinding.GlobalClass(_, _, arClass) =>
            for {
              (state, result) <- fromComp(createClassDefMessage(arClass).run(state))
              state2 <- f(state2, NonEmptyVector.of(result))

              instMethods <- fromComp(arClass.methods)
              (state, state2) <- processMethods(state, state2, instMethods)(f)

              staticMethods <- fromComp(arClass.staticMethods)
              (state, state2) <- processMethods(state, state2, staticMethods)(f)

              ctors <- fromComp(arClass.classConstructors)
              (state, state2) <- processClassCtors(state, state2, ctors)(f)

            } yield (state, state2)

          case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) =>
            for {
              (state, result) <- fromComp(createDataCtorDefMessage(dataCtor).run(state))
              state2 <- f(state2, NonEmptyVector.of(result))

              instMethods <- fromComp(dataCtor.methods)
              (state, state2) <- processMethods(state, state2, instMethods)(f)

            } yield (state, state2)

          case GlobalBinding.GlobalFunction(_, _, func) =>
            fromComp(createFuncDefMessage(func).run(state))
              .flatMap { case (state, result) =>
                f(state2, NonEmptyVector.of(result)).map { (state, _) }
              }

          case GlobalBinding.NestedNamespace(_, namespace) => processItem(state, state2, namespace)(f)
        }

      override protected def processItem[S2](state: ModuleIds, state2: S2, item: Namespace[context.type, DeclarationPayloadSpecifier])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(ModuleIds, S2)] =
        item.bindings.foldLeftM((state, state2)) { case ((state, state2), binding) => processBinding(state, state2, binding)(f) }

      override def processResult[S2](state: ModuleIds, state2: S2, result: Unit)(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
        ((), state2).point[F]
    }

  private def getObjId[D](descriptor: D)(nextId: Lens[ModuleIds, Int], idMap: Lens[ModuleIds, Map[D, Int]]): Emit[Int] =
    StateT.get[TComp, ModuleIds].flatMap { ids =>
      val map = idMap.get(ids)
      map.get(descriptor) match {
        case Some(id) => id.point[Emit]
        case None =>
          val id = nextId.get(ids)
          StateT.put[TComp, ModuleIds](
            idMap.set(nextId.set(ids)(id + 1))(map + (descriptor -> id))
          ).map { _ => id }
      }
    }

  private def getTraitId(descriptor: TraitDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextTraitId, lens[ModuleIds].traitIds)

  private def getClassId(descriptor: ClassDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextClassId, lens[ModuleIds].classIds)

  private def getDataCtorId(descriptor: DataConstructorDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextDataCtorId, lens[ModuleIds].dataCtorIds)

  private def getFuncId(descriptor: FuncDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextFunctionId, lens[ModuleIds].functionIds)

  private def getMethodId(descriptor: MethodDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextMethodId, lens[ModuleIds].methodIds)

  private def getClassCtorId(descriptor: ClassConstructorDescriptor): Emit[Int] =
    getObjId(descriptor)(lens[ModuleIds].nextClassCtorId, lens[ModuleIds].classCtorIds)


  private def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], A]
  (sig: Signature[TResult])
  (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.type]) => Emit[A])
  : Emit[A] = ???

  private def convertClassType(classType: typeSystem.ClassType): Emit[module.ClassType] = ???
  private def convertTraitType(traitType: typeSystem.TraitType): Emit[module.TraitType] = ???
  private def convertType(traitType: typeSystem.SimpleType): Emit[module.Type] = ???

  private def convertInNamespaceDescriptor(descriptor: InNamespaceDescriptor): module.InNamespaceDescriptor =
    module.InNamespaceDescriptor(
      fileId = descriptor.fileId.id,
      index = descriptor.index,
      ns = module.Namespace(descriptor.namespace.ns),
      name = module.GlobalName(descriptor.name match {
        case GlobalName.Normal(name) => module.GlobalName.GlobalName.NormalName(name)
        case GlobalName.Unnamed => module.GlobalName.GlobalName.Unnamed(1)
      }),
      accessModifier = convertAccessModifier(descriptor.accessModifier)
    )

  private def convertAccessModifier(accessModifier: AccessModifier): module.AccessModifier =
    accessModifier match {
      case AccessModifier.Public => module.AccessModifier.Public
      case AccessModifier.Internal => module.AccessModifier.Internal
      case AccessModifier.Protected => module.AccessModifier.Protected
      case AccessModifier.ProtectedInternal => module.AccessModifier.ProtectedInternal
      case AccessModifier.Private => module.AccessModifier.Private
      case AccessModifier.PrivateInternal => module.AccessModifier.PrivateInternal
    }

  private def convertTraitDescriptor(descriptor: TraitDescriptor): module.TraitDescriptor =
    module.TraitDescriptor(module.TraitDescriptor.Descriptor.InNamespace(
      descriptor match {
        case descriptor @ TraitDescriptor.InNamespace(_, _, _, _, _, _) => convertInNamespaceDescriptor(descriptor)
      }
    ))

  private def convertClassDescriptor(descriptor: ClassDescriptor): module.ClassDescriptor =
    module.ClassDescriptor(module.ClassDescriptor.Descriptor.InNamespace(
      descriptor match {
        case descriptor @ ClassDescriptor.InNamespace(_, _, _, _, _, _) => convertInNamespaceDescriptor(descriptor)
      }
    ))

  private def convertDataCtorDescriptor(descriptor: DataConstructorDescriptor): module.DataConstructorDescriptor =
    module.DataConstructorDescriptor(module.DataConstructorDescriptor.Descriptor.InNamespace(
      descriptor match {
        case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _, _, _) => convertInNamespaceDescriptor(descriptor)
      }
    ))

  private def convertMethodDescriptor(descriptor: MethodDescriptor): module.MethodDescriptor =
    module.MethodDescriptor(
      index = descriptor.index,
      accessModifier = convertAccessModifier(descriptor.accessModifier),
      memberName = descriptor.name match {
        case MemberName.Normal(name) => module.MethodDescriptor.MemberName.Name(name)
        case MemberName.Unnamed => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Unnamed)
        case MemberName.Call => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Call)
      },
      instanceType = descriptor.typeDescriptor match {
        case descriptor: TraitDescriptor => module.MethodDescriptor.InstanceType.TraitDescriptor(convertTraitDescriptor(descriptor))
        case TraitObjectDescriptor(descriptor) => module.MethodDescriptor.InstanceType.TraitObjectDescriptor(convertTraitDescriptor(descriptor))
        case descriptor: ClassDescriptor => module.MethodDescriptor.InstanceType.ClassDescriptor(convertClassDescriptor(descriptor))
        case ClassObjectDescriptor(descriptor) => module.MethodDescriptor.InstanceType.ClassObjectDescriptor(convertClassDescriptor(descriptor))
        case descriptor: DataConstructorDescriptor => module.MethodDescriptor.InstanceType.DataConstructorDescriptor(convertDataCtorDescriptor(descriptor))
      }
    )

  private def convertClassCtorDescriptor(descriptor: ClassConstructorDescriptor): module.ClassConstructorDescriptor =
    module.ClassConstructorDescriptor(
      ownerClass = convertClassDescriptor(descriptor.ownerClass),
      index = descriptor.index,
      accessModifier = convertAccessModifier(descriptor.accessModifier),
    )

  private def createTraitDefMessage(arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.TraitDefinition)] =
    for {
      sig <- emitComp(arTrait.signature)
      id <- getTraitId(arTrait.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        for {
          baseTraits <- result.baseTypes.baseTraits.traverse(convertTraitType(_))
        } yield module.TraitSignature(
          parameters = params,
          baseTraits = baseTraits,
        )
      }

      instMethods <- emitComp(arTrait.methods)
      staticMethods <- emitComp(arTrait.staticMethods)
      methods <- (instMethods ++ staticMethods).traverse { method =>
        getMethodId(method.descriptor).map { id =>
          module.MethodMember(convertMethodDescriptor(method.descriptor), id)
        }
      }

    } yield s"trait/$id" -> module.TraitDefinition(
      descriptor = convertTraitDescriptor(arTrait.descriptor),
      signature = convSig,
      isSealed = Some(arTrait.isSealed).filter(identity),
      methods = methods,
    )

  private def createClassDefMessage(arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.ClassDefinition)] =
    for {
      sig <- emitComp(arClass.signature)
      id <- getClassId(arClass.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        for {
          baseClass <- result.baseTypes.baseClass.traverse(convertClassType(_))
          baseTraits <- result.baseTypes.baseTraits.traverse(convertTraitType(_))
        } yield module.ClassSignature(
          parameters = params,
          baseClass = baseClass,
          baseTraits = baseTraits,
        )
      }
      fields <- emitComp(arClass.fields)
      convFields <- fields.traverse { field =>
        convertType(field.varType).map { fieldType =>
          module.ClassField(Mutability.toIsMutable(field.mutability), field.descriptor.name, fieldType)
        }
      }

      instMethods <- emitComp(arClass.methods)
      staticMethods <- emitComp(arClass.staticMethods)
      methods <- (instMethods ++ staticMethods).traverse { method =>
        getMethodId(method.descriptor).map { id =>
          module.MethodMember(convertMethodDescriptor(method.descriptor), id)
        }
      }

      classCtors <- emitComp(arClass.classConstructors)
      ctors <- classCtors.traverse { ctor =>
        getClassCtorId(ctor.descriptor).map { id =>
          module.ClassConstructorMember(convertClassCtorDescriptor(ctor.descriptor), id)
        }
      }
    } yield s"class/$id" -> module.ClassDefinition(
      descriptor = convertClassDescriptor(arClass.descriptor),
      signature = convSig,
      isOpen = Some(arClass.isOpen).filter(identity),
      isAbstract = Some(arClass.isAbstract).filter(identity),
      isSealed = Some(arClass.isSealed).filter(identity),
      fields = convFields
    )

  private def createDataCtorDefMessage(dataCtor: DataConstructor[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.DataConstructorDefinition)] =
    for {
      sig <- emitComp(dataCtor.signature)
      id <- getDataCtorId(dataCtor.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        for {
          instanceType <- convertTraitType(result.instanceType)
        } yield module.DataConstructorSignature(
          parameters = params,
          instanceType = instanceType,
        )
      }

      instMethods <- emitComp(dataCtor.methods)
      methods <- instMethods.traverse { method =>
        getMethodId(method.descriptor).map { id =>
          module.MethodMember(convertMethodDescriptor(method.descriptor), id)
        }
      }
    } yield s"dataCtor/$id" -> module.DataConstructorDefinition(
      descriptor = convertDataCtorDescriptor(dataCtor.descriptor),
      signature = convSig,
    )

  private def createFuncDefMessage(func: ArFunc[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.FunctionDefinition)] =
    for {
      sig <- emitComp(func.signature)
      id <- getFuncId(func.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        for {
          returnType <- convertType(result.returnType)
        } yield module.FunctionSignature(
          parameters = params,
          returnType = returnType,
        )
      }
    } yield s"func/$id" -> module.FunctionDefinition(
      descriptor = module.FunctionDescriptor(module.FunctionDescriptor.Descriptor.InNamespace(
        func.descriptor match {
          case descriptor @ FuncDescriptor.InNamespace(_, _, _, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = func.effectInfo.isPure
      ),
    )

  private def createMethodDefMessage(method: ArMethod[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.MethodDefinition)] =
    for {
      sig <- emitComp(method.signature)
      id <- getMethodId(method.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        for {
          returnType <- convertType(result.returnType)
        } yield module.MethodSignature(
          parameters = params,
          returnType = returnType,
        )
      }
    } yield s"func/$id" -> module.MethodDefinition(
      descriptor = convertMethodDescriptor(method.descriptor),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = method.effectInfo.isPure
      ),

      isVirtual = Some(method.isVirtual).filter(identity),
      isAbstract = Some(method.isAbstract).filter(identity),
      isImplicitOverride = Some(method.isImplicitOverride).filter(identity),
      isFinal = Some(method.isFinal).filter(identity),
    )

  private def createClassCtorDefMessage(ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.ClassConstructorDefinition)] =
    for {
      sig <- emitComp(ctor.signature)
      id <- getClassCtorId(ctor.descriptor)
      convSig <- convertSignature(sig) { (params, result) =>
        module.ClassConstructorSignature(params).point[Emit]
      }
    } yield s"classCtor/$id" -> module.ClassConstructorDefinition(
      descriptor = convertClassCtorDescriptor(ctor.descriptor),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = ctor.effectInfo.isPure
      ),
    )

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): ArStream[TComp, (String, GeneratedMessage), Unit] =
    ArStream.wrapEffect(
      module.globalNamespace.map { globalNS => ArStream.fromVector[TComp, Namespace[context.type, DeclarationPayloadSpecifier], Unit](Vector(globalNS), ()) }
    ).transformWith(globalTransform(NaturalTransformation.refl))

}
