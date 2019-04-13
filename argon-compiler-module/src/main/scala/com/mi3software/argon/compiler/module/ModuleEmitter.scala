package com.mi3software.argon.compiler.module

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import com.mi3software.argon.util.stream.{ArStream, StreamTransformation}
import scalapb.GeneratedMessage
import scalaz.{Lens => _, _}
import Scalaz._
import com.mi3software.argon.compiler.loaders.armodule.ModulePaths
import com.mi3software.argon.compiler.types.TypeSystem
import com.mi3software.argon.util.NonEmptyVector
import com.mi3software.argon.module
import shapeless._

final class ModuleEmitter[TComp[+_] : Compilation, TContext <: ModuleContext[TComp] with Singleton](context: TContext) {

  import context.signatureContext.Signature
  import context.typeSystem

  private object Implementation {

    final case class ModuleIds
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

      globals: Vector[module.GlobalDeclaration] = Vector(),

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

      emittedPaths: Set[String] = Set(),
    )

    final case class ModuleRefs
    (
      nextModuleId: Int = 1,
      moduleRefIds: Map[ModuleDescriptor, Int] = Map(),
    )

    type EmitF[F[_], A] = StateT[F, ModuleIds, A]
    type Emit[A] = EmitF[TComp, A]

    def emitComp[A](comp: TComp[A]): Emit[A] = StateT.liftM(comp)


    def globalTransform[F[_]](fromComp: TComp ~> F): StreamTransformation[F, ArModule[context.type, DeclarationPayloadSpecifier], Unit, (String, GeneratedMessage), Unit] =
      new StreamTransformation.Single[F, ArModule[context.type, DeclarationPayloadSpecifier], Unit, (String, GeneratedMessage), Unit] {
        override type S = Unit
        override val initialState: Unit = ()

        private def fromEmit[A](e: Emit[A])(implicit monadInstance: Monad[F]): EmitF[F, A] =
          StateT.hoist(fromComp).apply(e)

        private def emitF[A](fa: F[A])(implicit monadInstance: Monad[F]): EmitF[F, A] =
          StateT.liftM(fa)

        private def addEmitId(id: String)(implicit monadInstance: Monad[F]): EmitF[F, Unit] =
          StateT.modify[F, ModuleIds](s => s.copy(emittedPaths = s.emittedPaths + id))

        private def processMethods[S2](state2: S2, armodule: ArModule[context.type, DeclarationPayloadSpecifier], methods: Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): EmitF[F, S2] =
          methods.foldLeftM(state2) { case (state2, MethodBinding(_, _, _, method)) =>
            for {
              result <- fromEmit(createMethodDefMessage(armodule, method))
              state2 <- emitF(f(state2, NonEmptyVector.of(result)))
              _ <- addEmitId(result._1)
            } yield state2
          }

        private def processClassCtors[S2](state2: S2, armodule: ArModule[context.type, DeclarationPayloadSpecifier], ctors: Vector[ClassConstructorBinding[context.type, DeclarationPayloadSpecifier]])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): EmitF[F, S2] =
          ctors.foldLeftM(state2) { case (state2, ClassConstructorBinding(_, _, ctor)) =>
            for {
              result <- fromEmit(createClassCtorDefMessage(armodule, ctor))
              state2 <- emitF(f(state2, NonEmptyVector.of(result)))
              _ <- addEmitId(result._1)
            } yield state2
          }

        private def processBinding[S2](armodule: ArModule[context.type, DeclarationPayloadSpecifier], state2: S2, binding: GlobalBinding[context.type, DeclarationPayloadSpecifier])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): EmitF[F, S2] = {

          def addGlobalDecl(g: module.GlobalDeclaration): EmitF[F, Unit] =
            StateT.modify[F, ModuleIds](s => s.copy(globals = s.globals :+ g))

          def bindingCommon[V, M <: GeneratedMessage]
          (access: AccessModifierGlobal)
          (value: V)
          (getId: EmitF[F, Int])
          (getPath: Int => String)
          (createMessage: (ArModule[context.type, DeclarationPayloadSpecifier], V) => Emit[M])
          (descriptor: module.GlobalDeclaration.Descriptor)
          (emitExtra: S2 => EmitF[F, S2])
          : EmitF[F, S2] =
            getId.flatMap { id =>
              val path = getPath(id)

              StateT.get[F, ModuleIds].flatMap { state =>
                if(state.emittedPaths.contains(path))
                  state2.point[EmitF[F, ?]]
                else
                  for {
                    result <- fromEmit(createMessage(armodule, value))
                    state2 <- emitF(f(state2, NonEmptyVector.of(path -> result)))
                    _ <- addEmitId(path)

                    state2 <- emitExtra(state2)

                    _ <- addGlobalDecl(module.GlobalDeclaration(id, convertAccessModifier(access), descriptor))

                  } yield state2
              }
            }


          binding match {
            case GlobalBinding.GlobalTrait(_, access, arTrait) =>
              bindingCommon(access)(arTrait)(getTraitId[F](armodule, arTrait.descriptor))(ModulePaths.traitDef)(createTraitDefMessage)(
                module.GlobalDeclaration.Descriptor.TraitDescriptor(convertInNamespaceDescriptor(arTrait.descriptor))
              ) { state2 =>
                for {
                  instMethods <- emitF(fromComp(arTrait.methods))
                  state2 <- processMethods(state2, armodule, instMethods)(f)

                  staticMethods <- emitF(fromComp(arTrait.staticMethods))
                  state2 <- processMethods(state2, armodule, staticMethods)(f)
                } yield state2
              }

            case GlobalBinding.GlobalClass(_, access, arClass) =>
              bindingCommon(access)(arClass)(getClassId[F](armodule, arClass.descriptor))(ModulePaths.classDef)(createClassDefMessage)(
                module.GlobalDeclaration.Descriptor.ClassDescriptor(convertInNamespaceDescriptor(arClass.descriptor))
              ) { state2 =>
                for {
                  instMethods <- emitF(fromComp(arClass.methods))
                  state2 <- processMethods(state2, armodule, instMethods)(f)

                  staticMethods <- emitF(fromComp(arClass.staticMethods))
                  state2 <- processMethods(state2, armodule, staticMethods)(f)

                  ctors <- emitF(fromComp(arClass.classConstructors))
                  state2 <- processClassCtors(state2, armodule, ctors)(f)
                } yield state2
              }

            case GlobalBinding.GlobalDataConstructor(_, access, dataCtor) =>
              bindingCommon(access)(dataCtor)(getDataCtorId[F](armodule, dataCtor.descriptor))(ModulePaths.dataCtorDef)(createDataCtorDefMessage)(
                module.GlobalDeclaration.Descriptor.DataConstructorDescriptor(convertInNamespaceDescriptor(dataCtor.descriptor))
              ) { state2 =>
                for {
                  instMethods <- emitF(fromComp(dataCtor.methods))
                  state2 <- processMethods(state2, armodule, instMethods)(f)
                } yield state2
              }

            case GlobalBinding.GlobalFunction(_, access, func) =>
              bindingCommon(access)(func)(getFuncId[F](armodule, func.descriptor))(ModulePaths.funcDef)(createFuncDefMessage)(
                module.GlobalDeclaration.Descriptor.FunctionDescriptor(convertInNamespaceDescriptor(func.descriptor))
              ) { state2 =>
                state2.point[EmitF[F, ?]]
              }

            case GlobalBinding.NestedNamespace(_, namespace) => processNamespace(armodule, state2, namespace)(f)
          }

        }

        private def processNamespace[S2](armodule: ArModule[context.type, DeclarationPayloadSpecifier], state2: S2, item: Namespace[context.type, DeclarationPayloadSpecifier])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): EmitF[F, S2] =
          item.bindings.foldLeftM(state2) { (state2, binding) => processBinding(armodule, state2, binding)(f) }

        private def pairState[A, B, C](s: StateT[F, A, C])(lens: Lens[B, A])(implicit monadInstance: Monad[F]): StateT[F, B, C] =
          StateT[F, B, C] { b =>
            s.run(lens.get(b)).map { case (a, c) =>
              (lens.set(b)(a), c)
            }
          }

        private trait RefEmitHandler {
          def emitRefs[S2](armodule: ArModule[context.type, DeclarationPayloadSpecifier], state2: S2, f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2]): StateT[F, (ModuleIds, ModuleRefs), S2]
        }

        private def emitRefsSimple[D1 <: Descriptor, D2, R <: GeneratedMessage]
        (createPath: Int => String)
        (refIds: ModuleIds => Map[D1, Int])
        (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => EmitF[F, D2])
        (createRef: (Int, D2) => R)
        (implicit monadInstance: Monad[F])
        : RefEmitHandler =
          emitRefs(createPath)(refIds)(convertDescriptor)(createRef)

        private def emitRefs[D1 <: Descriptor, D2, R <: GeneratedMessage]
        (createPath: Int => String)
        (refIds: ModuleIds => Map[D1, Int])
        (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => EmitF[F, D2])
        (createRef: (Int, D2) => R)
        (implicit monadInstance: Monad[F])
        : RefEmitHandler = new RefEmitHandler {
          override def emitRefs[S2](armodule: ArModule[context.type, DeclarationPayloadSpecifier], state2: S2, f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2]): StateT[F, (ModuleIds, ModuleRefs), S2] =
            StateT.get[F, (ModuleIds, ModuleRefs)].flatMap { case (moduleIds, _) =>
              refIds(moduleIds).toVector
                .foldLeftM(state2) { case (state2, (desc, id)) =>
                  for {
                    moduleId <- pairState(getModuleId[F](desc.moduleDescriptor))(lens[(ModuleIds, ModuleRefs)]._2)
                    _ <- StateT.modify[F, (ModuleIds, ModuleRefs)] { case (moduleIds, moduleRefs) =>
                      (moduleIds, moduleRefs.copy(moduleRefIds = moduleRefs.moduleRefIds + (desc.moduleDescriptor -> moduleId)))
                    }

                    convDesc <- pairState(convertDescriptor(armodule, desc))(lens[(ModuleIds, ModuleRefs)]._1)
                    ref = createRef(moduleId, convDesc)

                    state2 <- StateT.liftM[F, (ModuleIds, ModuleRefs), S2](f(state2, NonEmptyVector.of(createPath(id.abs) -> ref)))
                  } yield state2
                }
            }
        }

        private def emitAllRefs[S2]
        (armodule: ArModule[context.type, DeclarationPayloadSpecifier])
        (state2: S2)
        (f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])
        (emitHandlers: RefEmitHandler*)
        (implicit monadInstance: Monad[F])
        : StateT[F, ModuleIds, (ModuleRefs, S2)] = {
          def impl(moduleRefs: ModuleRefs, state2: S2): StateT[F, ModuleIds, (ModuleRefs, S2)] =
            StateT.get[F, ModuleIds].flatMap { origState =>
              emitHandlers.toVector.foldLeftM((moduleRefs, state2)) { case ((moduleRefs, state2), handler) =>
                StateT[F, ModuleIds, (ModuleRefs, S2)](state => handler.emitRefs(armodule, state2, f).run((state, moduleRefs)).map {
                  case ((moduleIds, moduleRefs), state2) =>
                    (moduleIds, (moduleRefs, state2))
                })
              }
                .flatMap { case (moduleRefs, state2) =>
                  StateT.get[F, ModuleIds].flatMap { newState =>
                    if(origState.emittedPaths.size =/= newState.emittedPaths.size)
                      impl(moduleRefs, state2)
                    else
                      (moduleRefs, state2).point[StateT[F, ModuleIds, ?]]
                  }
                }
            }

          impl(ModuleRefs(), state2)
        }

        override protected def processItem[S2](state: Unit, state2: S2, item: ArModule[context.type, DeclarationPayloadSpecifier])(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
          (
            for {
              ns <- emitF(fromComp(item.globalNamespace))
              state2 <- processNamespace(item, state2, ns)(f)
              (moduleRefs, state2) <- emitAllRefs(item)(state2)(f)(
                emitRefs(ModulePaths.methodRef)(_.methodRefIds)(convertMethodDescriptor[F])((_, _) => ???),
                emitRefsSimple(ModulePaths.classCtorRef)(_.classCtorRefIds)(convertClassCtorDescriptor[F])(module.ClassConstructorReference.apply),
                emitRefsSimple(ModulePaths.traitRef)(_.traitRefIds)(convertTraitDescriptor[F])(module.TraitReference.apply),
                emitRefsSimple(ModulePaths.classRef)(_.classRefIds)(convertClassDescriptor[F])(module.ClassReference.apply),
                emitRefsSimple(ModulePaths.dataCtorRef)(_.dataCtorRefIds)(convertDataCtorDescriptor[F])(module.DataConstructorReference.apply),
                emitRefsSimple(ModulePaths.funcRef)(_.functionRefIds)(convertFuncDescriptor[F])(module.FunctionReference.apply),
              )
              moduleIds <- StateT.get[F, ModuleIds]
              state2 <- emitF(f(state2, NonEmptyVector.of(ModulePaths.metadata -> module.Metadata(
                formatVersion = 1,
                name = item.descriptor.name,
                references =
                  moduleRefs.moduleRefIds
                    .toVector
                    .sortBy { case (_, id) => id }
                    .map { case (ModuleDescriptor(name), _) => module.ModuleReference(name) },
                globals = moduleIds.globals
              ))))
            } yield ((), state2)
          ).eval(ModuleIds())

        override def processResult[S2](state: Unit, state2: S2, result: Unit)(f: (S2, NonEmptyVector[(String, GeneratedMessage)]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
          ((), state2).point[F]
      }

    def getObjId[F[_]: Monad, O, D](descriptor: D)(nextId: Lens[O, Int], idMap: Lens[O, Map[D, Int]]): StateT[F, O, Int] =
      StateT.get[F, O].flatMap { ids =>
        val map = idMap.get(ids)
        map.get(descriptor) match {
          case Some(id) => id.point[StateT[F, O, ?]]
          case None =>
            val id = nextId.get(ids)
            StateT.put[F, O](
              idMap.set(nextId.set(ids)(id + 1))(map + (descriptor -> id))
            ).map { _ => id }
        }
      }

    def getModuleId[F[_]: Monad](descriptor: ModuleDescriptor): StateT[F, ModuleRefs, Int] =
      getObjId(descriptor)(lens[ModuleRefs].nextModuleId, lens[ModuleRefs].moduleRefIds)

    def getTraitId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjId(descriptor)(lens[ModuleIds].nextTraitId, lens[ModuleIds].traitIds)
      else
        getObjId(descriptor)(lens[ModuleIds].nextTraitRefId, lens[ModuleIds].traitRefIds)


    def getClassId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjId(descriptor)(lens[ModuleIds].nextClassId, lens[ModuleIds].classIds)
      else
        getObjId(descriptor)(lens[ModuleIds].nextClassRefId, lens[ModuleIds].classRefIds)

    def getDataCtorId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjId(descriptor)(lens[ModuleIds].nextDataCtorId, lens[ModuleIds].dataCtorIds)
      else
        getObjId(descriptor)(lens[ModuleIds].nextDataCtorRefId, lens[ModuleIds].dataCtorRefIds)

    def getFuncId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjId(descriptor)(lens[ModuleIds].nextFunctionId, lens[ModuleIds].functionIds)
      else
        getObjId(descriptor)(lens[ModuleIds].nextFunctionRefId, lens[ModuleIds].functionRefIds)

    def getMethodId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: MethodDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjId(descriptor)(lens[ModuleIds].nextMethodId, lens[ModuleIds].methodIds)
      else
        getObjId(descriptor)(lens[ModuleIds].nextMethodRefId, lens[ModuleIds].methodRefIds)

    def getClassCtorId[F[_]: Monad](descriptor: ClassConstructorDescriptor): EmitF[F, Int] =
      getObjId(descriptor)(lens[ModuleIds].nextClassCtorId, lens[ModuleIds].classCtorIds)


    def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], A]
    (armodule: ArModule[context.type, DeclarationPayloadSpecifier])
    (sig: Signature[TResult])
    (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.type]) => Emit[A])
    : Emit[A] = {

      def impl(sig: Signature[TResult], prevParams: Vector[module.Parameter]): Emit[A] =
        sig.visit(
          sigParams =>
            sigParams.parameter.tupleVars
              .traverse { elem =>
                convertType(armodule, elem.varType).map { paramType =>
                  module.ParameterElement(
                    elem.name match {
                      case VariableName.Normal(name) => Some(name)
                      case VariableName.Unnamed => None
                    },
                    paramType
                  )
                }
              }
              .flatMap { elems =>
                impl(sig, prevParams :+ module.Parameter(elems))
              },
          sigResult => f(prevParams, sigResult.result),
        )

      impl(sig, Vector.empty)
    }

    def convertClassType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], classType: typeSystem.ClassType): Emit[module.ClassType] = for {
      id <- getClassId[TComp](armodule, classType.arClass.value.descriptor)
      args <- classType.args.traverse(convertType(armodule, _))
    } yield module.ClassType(id, args)

    def convertTraitType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], traitType: typeSystem.TraitType): Emit[module.TraitType] = for {
      id <- getTraitId[TComp](armodule, traitType.arTrait.value.descriptor)
      args <- traitType.args.traverse(convertType(armodule, _))
    } yield module.TraitType(id, args)

    def convertDataCtorType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], dataCtorType: typeSystem.DataConstructorType): Emit[module.DataConstructorType] = for {
      id <- getDataCtorId[TComp](armodule, dataCtorType.ctor.value.descriptor)
      args <- dataCtorType.args.traverse(convertType(armodule, _))
    } yield module.DataConstructorType(id, args)

    def convertType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], t: typeSystem.SimpleType): Emit[module.Type] =
      ((t match {
        case t: typeSystem.ClassType => convertClassType(armodule, t).map(module.Type.TypeInfo.ClassType)
        case t: typeSystem.TraitType => convertTraitType(armodule, t).map(module.Type.TypeInfo.TraitType)
        case t: typeSystem.DataConstructorType => convertDataCtorType(armodule, t).map(module.Type.TypeInfo.DataConstructorType)
        case _ => ???
      }) : Emit[module.Type.TypeInfo])
        .map(module.Type.apply)

    def convertInNamespaceDescriptor(descriptor: InNamespaceDescriptor): module.InNamespaceDescriptor =
      module.InNamespaceDescriptor(
        ns = module.Namespace(descriptor.namespace.ns),
        name = module.GlobalName(descriptor.name match {
          case GlobalName.Normal(name) => module.GlobalName.GlobalName.NormalName(name)
          case GlobalName.Unnamed => module.GlobalName.GlobalName.Unnamed(1)
        }),
      )

    def convertAccessModifier(accessModifier: AccessModifier): module.AccessModifier =
      accessModifier match {
        case AccessModifier.Public => module.AccessModifier.Public
        case AccessModifier.Internal => module.AccessModifier.Internal
        case AccessModifier.Protected => module.AccessModifier.Protected
        case AccessModifier.ProtectedInternal => module.AccessModifier.ProtectedInternal
        case AccessModifier.Private => module.AccessModifier.Private
        case AccessModifier.PrivateInternal => module.AccessModifier.PrivateInternal
      }

    def convertTraitDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): EmitF[F, module.TraitDescriptor] =
      module.TraitDescriptor(module.TraitDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ TraitDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).point[EmitF[F, ?]]

    def convertClassDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): EmitF[F, module.ClassDescriptor] =
      module.ClassDescriptor(module.ClassDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ ClassDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).point[EmitF[F, ?]]

    def convertDataCtorDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): EmitF[F, module.DataConstructorDescriptor] =
      module.DataConstructorDescriptor(module.DataConstructorDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).point[EmitF[F, ?]]

    def convertFuncDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): EmitF[F, module.FunctionDescriptor] =
      module.FunctionDescriptor(module.FunctionDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ FuncDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).point[EmitF[F, ?]]

    def convertMethodDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: MethodDescriptor): EmitF[F, module.MethodDescriptor] =
      for {
        (instanceType, parentId) <- descriptor.typeDescriptor match {
          case descriptor: TraitDescriptor =>
            for {
              desc <- convertTraitDescriptor[F](armodule, descriptor)
              parentId <- getTraitId[F](armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.TraitDescriptor(desc), parentId)

          case TraitObjectDescriptor(descriptor) =>
            for {
              desc <- convertTraitDescriptor[F](armodule, descriptor)
              parentId <- getTraitId[F](armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.TraitObjectDescriptor(desc), parentId)

          case descriptor: ClassDescriptor =>
            for {
              desc <- convertClassDescriptor[F](armodule, descriptor)
              parentId <- getClassId[F](armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.ClassDescriptor(desc), parentId)

          case ClassObjectDescriptor(descriptor) =>
            for {
              desc <- convertClassDescriptor[F](armodule, descriptor)
              parentId <- getClassId[F](armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.ClassObjectDescriptor(desc), parentId)

          case descriptor: DataConstructorDescriptor =>
            for {
              desc <- convertDataCtorDescriptor[F](armodule, descriptor)
              parentId <- getDataCtorId[F](armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.DataConstructorDescriptor(desc), parentId)
        }
      } yield module.MethodDescriptor(
        index = descriptor.index,
        instanceTypeId = parentId,
        memberName = descriptor.name match {
          case MemberName.Normal(name) => module.MethodDescriptor.MemberName.Name(name)
          case MemberName.Unnamed => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Unnamed)
          case MemberName.Call => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Call)
        },
        instanceType = instanceType
      )

    def convertClassCtorDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassConstructorDescriptor): EmitF[F, module.ClassConstructorDescriptor] =
      for {
        ownerClass <- convertClassDescriptor[F](armodule, descriptor.ownerClass)
      } yield module.ClassConstructorDescriptor(
        ownerClass = ownerClass,
        index = descriptor.index,
      )

    def createTraitDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Emit[module.TraitDefinition] =
      for {
        sig <- emitComp(arTrait.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            baseTraits <- result.baseTypes.baseTraits.traverse(convertTraitType(armodule, _))
          } yield module.TraitSignature(
            parameters = params,
            baseTraits = baseTraits,
          )
        }

        instMethods <- emitComp(arTrait.methods)
        staticMethods <- emitComp(arTrait.staticMethods)
        methods <- (instMethods ++ staticMethods).traverse { method =>
          convertMethodDescriptor[TComp](armodule, method.method.descriptor).map { convDesc =>
            module.MethodMember(convDesc, method.index, convertAccessModifier(method.accessModifier))
          }
        }

        convDesc <- convertTraitDescriptor[TComp](armodule, arTrait.descriptor)

      } yield module.TraitDefinition(
        descriptor = convDesc,
        signature = convSig,
        isSealed = Some(arTrait.isSealed).filter(identity),
        methods = methods,
      )

    def createClassDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Emit[module.ClassDefinition] =
      for {
        sig <- emitComp(arClass.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            baseClass <- result.baseTypes.baseClass.traverse(convertClassType(armodule, _))
            baseTraits <- result.baseTypes.baseTraits.traverse(convertTraitType(armodule, _))
          } yield module.ClassSignature(
            parameters = params,
            baseClass = baseClass,
            baseTraits = baseTraits,
          )
        }
        fields <- emitComp(arClass.fields)
        convFields <- fields.traverse { field =>
          convertType(armodule, field.varType).map { fieldType =>
            module.ClassField(Mutability.toIsMutable(field.mutability), field.descriptor.name, fieldType)
          }
        }

        instMethods <- emitComp(arClass.methods)
        staticMethods <- emitComp(arClass.staticMethods)
        methods <- (instMethods ++ staticMethods).traverse { method =>
          convertMethodDescriptor[TComp](armodule, method.method.descriptor).map { convDesc =>
            module.MethodMember(convDesc, method.index, convertAccessModifier(method.accessModifier))
          }
        }

        classCtors <- emitComp(arClass.classConstructors)
        ctors <- classCtors.traverse { ctor =>
          convertClassCtorDescriptor[TComp](armodule, ctor.ctor.descriptor).map { convDesc =>
            module.ClassConstructorMember(convDesc, ctor.index)
          }
        }

        convDesc <- convertClassDescriptor[TComp](armodule, arClass.descriptor)

      } yield module.ClassDefinition(
        descriptor = convDesc,
        signature = convSig,
        isOpen = Some(arClass.isOpen).filter(identity),
        isAbstract = Some(arClass.isAbstract).filter(identity),
        isSealed = Some(arClass.isSealed).filter(identity),
        fields = convFields
      )

    def createDataCtorDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], dataCtor: DataConstructor[context.type, DeclarationPayloadSpecifier]): Emit[module.DataConstructorDefinition] =
      for {
        sig <- emitComp(dataCtor.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            instanceType <- convertTraitType(armodule, result.instanceType)
          } yield module.DataConstructorSignature(
            parameters = params,
            instanceType = instanceType,
          )
        }

        instMethods <- emitComp(dataCtor.methods)
        methods <- instMethods.traverse { method =>
          convertMethodDescriptor[TComp](armodule, method.method.descriptor).map { convDesc =>
            module.MethodMember(convDesc, method.index, convertAccessModifier(method.accessModifier))
          }
        }

        convDesc <- convertDataCtorDescriptor[TComp](armodule, dataCtor.descriptor)
        ctorDef = module.DataConstructorDefinition(
          descriptor = convDesc,
          signature = convSig,
        )

      } yield ctorDef

    def createFuncDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], func: ArFunc[context.type, DeclarationPayloadSpecifier]): Emit[module.FunctionDefinition] =
      for {
        sig <- emitComp(func.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            returnType <- convertType(armodule, result.returnType)
          } yield module.FunctionSignature(
            parameters = params,
            returnType = returnType,
          )
        }

        convDesc <- convertFuncDescriptor[TComp](armodule, func.descriptor)

      } yield module.FunctionDefinition(
        descriptor = convDesc,
        signature = convSig,
        effects = module.EffectInfo(
          isPure = func.effectInfo.isPure
        ),
      )

    def createMethodDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], method: ArMethod[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.MethodDefinition)] =
      for {
        sig <- emitComp(method.signature)
        id <- getMethodId[TComp](armodule, method.descriptor)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            returnType <- convertType(armodule, result.returnType)
          } yield module.MethodSignature(
            parameters = params,
            returnType = returnType,
          )
        }

        methodOwner <- method.owner match {
          case ArMethod.ClassOwner(ownerClass) => getClassId[TComp](armodule, ownerClass.descriptor).map(module.MethodDefinition.MethodOwner.OwnerClassId.apply)
          case ArMethod.ClassObjectOwner(ownerClass) => getClassId[TComp](armodule, ownerClass.descriptor).map(module.MethodDefinition.MethodOwner.OwnerClassObjectId.apply)
          case ArMethod.TraitOwner(ownerTrait) => getTraitId[TComp](armodule, ownerTrait.descriptor).map(module.MethodDefinition.MethodOwner.OwnerTraitId.apply)
          case ArMethod.TraitObjectOwner(ownerTrait) => getTraitId[TComp](armodule, ownerTrait.descriptor).map(module.MethodDefinition.MethodOwner.OwnerTraitObjectId.apply)
          case ArMethod.DataCtorOwner(dataCtor) => getDataCtorId[TComp](armodule, dataCtor.descriptor).map(module.MethodDefinition.MethodOwner.OwnerConstructorId.apply)
        }

        convDesc <- convertMethodDescriptor[TComp](armodule, method.descriptor)
        methodDef = module.MethodDefinition(
          descriptor = convDesc,
          signature = convSig,
          effects = module.EffectInfo(
            isPure = method.effectInfo.isPure
          ),

          methodOwner = methodOwner,

          isVirtual = Some(method.isVirtual).filter(identity),
          isAbstract = Some(method.isAbstract).filter(identity),
          isImplicitOverride = Some(method.isImplicitOverride).filter(identity),
          isFinal = Some(method.isFinal).filter(identity),
        )

      } yield ModulePaths.methodDef(id) -> methodDef

    def createClassCtorDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.ClassConstructorDefinition)] =
      for {
        sig <- emitComp(ctor.signature)
        id <- getClassCtorId[TComp](ctor.descriptor)
        convSig <- convertSignature(armodule)(sig) { (params, _) =>
          module.ClassConstructorSignature(params).point[Emit]
        }

        convDesc <- convertClassCtorDescriptor[TComp](armodule, ctor.descriptor)
        classCtorDef = module.ClassConstructorDefinition(
          descriptor = convDesc,
          signature = convSig,
          effects = module.EffectInfo(
            isPure = ctor.effectInfo.isPure
          ),
        )

      } yield ModulePaths.classCtorDef(id) -> classCtorDef


  }

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): ArStream[TComp, (String, GeneratedMessage), Unit] =
    ArStream.fromVector[TComp, ArModule[context.type, DeclarationPayloadSpecifier], Unit](Vector(module), ())
      .transformWith(Implementation.globalTransform(NaturalTransformation.refl))

}
