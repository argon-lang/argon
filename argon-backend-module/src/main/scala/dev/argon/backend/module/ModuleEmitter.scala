package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats._
import cats.data._
import cats.implicits._
import dev.argon.compiler.loaders.armodule.{ModuleFormatVersion, ModulePaths}
import dev.argon.compiler.types.TypeSystem
import dev.argon.util.FileID
import dev.argon.module
import dev.argon.stream.{ArStream, Resource, Step, StreamTransformation}
import shapeless._
import dev.argon.util.AnyExtensions._
import zio.stream.ZStream.Fold
import zio.{IO, ZIO, stream => zstream}

final class ModuleEmitter[TCompRE[-_, +_, +_], R, TContext <: ModuleContext[TCompRE, R, _] with Singleton](val context: TContext) {

  type TCompE[+E, +A] = context.CompE[E, A]
  type TComp[+A] = context.Comp[A]

  import context._
  import context.signatureContext.Signature

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
      methodRefIds: Map[MethodDescriptor, (Int, module.MethodReference.MethodOwner)] = Map(),
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

    def emitComp[A](comp: TComp[A]): Emit[A] = StateT.liftF[TComp, ModuleIds, A](comp)

    implicit val fromCompRefl: TComp ~> TComp = arrow.FunctionK.id

    def moduleBindingStream[F[-_, +_, +_], R2 <: R, E >: NonEmptyList[CompilationError]](fromComp: ArStream.EffectConverter[TCompRE, F])(armodule: ArModule[context.type, DeclarationPayloadSpecifier]): ArStream[F, R2, E, GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
      new ArStream[F, R2, E, GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] {

        type FE[+A] = F[R, E, A]

        override def foldLeft[R3 <: R2, E2 >: E, A2 >: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], X]
        (trans: StreamTransformation[F, R3, E2, A2, Unit, Nothing, X])
        (implicit monadInstance: Monad[F[R3, E2, ?]])
        : F[R3, E2, X] =
          (fromComp(armodule.globalNamespace) : F[R3, E2, Namespace[context.type, DeclarationPayloadSpecifier]]).flatMap { ns =>
            trans.initial.use { state =>
              processNamespace(trans)(state, ns).flatMap {
                case Step.Produce(_, value, _) => value
                case Step.Continue(state) => trans.end(state, ()).flatMap { case (_, fr2) => fr2 }
                case Step.Stop(r2) => r2.pure[F[R3, E2, ?]]
              }
            }
          }


        private def processNamespace[R3 <: R2, E2 >: E, A2 >: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], X]
        (trans: StreamTransformation[F, R3, E2, A2, Unit, Nothing, X])
        (state: trans.State, ns: Namespace[context.type, DeclarationPayloadSpecifier])
        (implicit monadInstance: Monad[F[R3, E2, ?]])
        : F[R3, E2, Step[trans.State, A2, Nothing, X]] = {

          def feed(bindings: Vector[GlobalBinding[context.type, DeclarationPayloadSpecifier]], state: trans.State): F[R3, E2, Step[trans.State, A2, Nothing, X]] =
            bindings match {
              case Vector() => Monad[F[R3, E2, ?]].pure(Step.Continue(state))
              case head +: tail =>
                processBinding[R3, E2, A2, X](trans)(state, head).flatMap {
                  case Step.Produce(_, value, _) => value
                  case Step.Continue(state) => feed(tail, state)
                  case stop @ Step.Stop(_) => Monad[F[R3, E2, ?]].pure(stop)
                }
            }

          feed(ns.bindings, state)
        }

        private def processBinding[R3 <: R2, E2 >: E, A2 >: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], X]
        (trans: StreamTransformation[F, R3, E2, A2, Unit, Nothing, X])
        (state: trans.State, binding: GlobalBinding[context.type, DeclarationPayloadSpecifier])
        (implicit monadInstance: Monad[F[R3, E2, ?]])
        : F[R3, E2, Step[trans.State, A2, Nothing, X]] =
          binding match {
            case binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] => trans.step(state, NonEmptyVector.of(binding)).map(identity)
            case GlobalBinding.NestedNamespace(_, namespace) => processNamespace(trans)(state, namespace)
          }


      }

    def moduleEmitTransform[F[-_, +_, +_]]
    (fromComp: ArStream.EffectConverter[TCompRE, F])
    (armodule: ArModule[context.type, DeclarationPayloadSpecifier])
    (implicit monadInstance: Monad[F[R, NonEmptyList[CompilationError], ?]])
    : StreamTransformation[F, R, NonEmptyList[CompilationError], GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], Unit, Vector[(String, GeneratedMessage)], Unit] =
      new StreamTransformation.Single[F, R, NonEmptyList[CompilationError], GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], Unit, Vector[(String, GeneratedMessage)], Unit] {
        override type State = ModuleIds

        type E = NonEmptyList[CompilationError]
        type FE[+X] = F[R, E, X]

        override def initial: Resource[F, R, E, ModuleIds] = Resource.pure(ModuleIds())

        def fromEmit[A](e: Emit[A]): EmitF[FE, A] =
          e.mapK[FE](fromComp.toFunctionK[R, E])

        def emitF[A](fa: FE[A]): EmitF[FE, A] =
          StateT.liftF(fa)

        def addEmitId(id: String): EmitF[FE, Unit] =
          StateT.modify[FE, ModuleIds](s => s.copy(emittedPaths = s.emittedPaths + id))

        override def stepSingle
        (s: ModuleIds, a: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier])
        : FE[Step[ModuleIds, GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier], Vector[(String, GeneratedMessage)], Unit]] =
          processBinding(a).run(s).map {
            case (s, result) => Step.Produce(s, result, Vector.empty)
          }


        override def end(s: ModuleIds, result: Unit): FE[(Vector[Vector[(String, GeneratedMessage)]], FE[Unit])] =
          (
            for {
              (moduleRefs, refEntries) <- emitAllRefs(
                emitRefs(_.methodRefIds)({ case (id, _) => ModulePaths.methodRef(id) })(convertMethodDescriptor[FE]) {
                  case (moduleId, desc, (_, owner)) =>
                    module.MethodReference(moduleId, desc, owner)
                },
                emitRefsSimple(_.classCtorRefIds)(ModulePaths.classCtorRef)(convertClassCtorDescriptor[FE])(module.ClassConstructorReference.apply),
                emitRefsSimple(_.traitRefIds)(ModulePaths.traitRef)(convertTraitDescriptor[FE])(module.TraitReference.apply),
                emitRefsSimple(_.classRefIds)(ModulePaths.classRef)(convertClassDescriptor[FE])(module.ClassReference.apply),
                emitRefsSimple(_.dataCtorRefIds)(ModulePaths.dataCtorRef)(convertDataCtorDescriptor[FE])(module.DataConstructorReference.apply),
                emitRefsSimple(_.functionRefIds)(ModulePaths.funcRef)(convertFuncDescriptor[FE])(module.FunctionReference.apply),
              )

              moduleIds <- StateT.get[FE, ModuleIds]
              metadataEntry = Vector(ModulePaths.metadata -> module.Metadata(
                formatVersion = ModuleFormatVersion.currentVersion,
                name = armodule.descriptor.name,
                references =
                  moduleRefs.moduleRefIds
                    .toVector
                    .sortBy { case (_, id) => id }
                    .map { case (ModuleDescriptor(name), _) => module.ModuleReference(name) },
                globals = moduleIds.globals
              ))
            } yield (refEntries :+ metadataEntry, ().pure[FE])
          ).runA(s)



        def addGlobalDecl(g: module.GlobalDeclaration): EmitF[FE, Unit] =
          StateT.modify[FE, ModuleIds](s => s.copy(globals = s.globals :+ g))

        def bindingCommon[V, M <: GeneratedMessage]
        (access: AccessModifierGlobal)
        (value: V)
        (getId: EmitF[FE, Int])
        (getPath: Int => String)
        (createMessage: (ArModule[context.type, DeclarationPayloadSpecifier], V) => Emit[M])
        (descriptor: module.GlobalDeclaration.Descriptor)
        (emitExtra: EmitF[FE, Vector[(String, GeneratedMessage)]])
        : EmitF[FE, Vector[(String, GeneratedMessage)]] =
          getId.flatMap { id =>
            val path = getPath(id)

            StateT.get[FE, ModuleIds].flatMap { state =>
              if(state.emittedPaths.contains(path))
                StateT.pure[FE, ModuleIds, Vector[(String, GeneratedMessage)]](Vector.empty)
              else
                for {
                  result <- fromEmit(createMessage(armodule, value))
                  _ <- addEmitId(path)
                  extra <- emitExtra

                  _ <- addGlobalDecl(module.GlobalDeclaration(id, convertAccessModifier(access), descriptor))

                } yield (path -> result) +: extra
            }
          }


        def processBinding(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): EmitF[FE, Vector[(String, GeneratedMessage)]] =
          binding match {
            case GlobalBinding.GlobalTrait(_, access, arTrait) =>
              bindingCommon(access)(arTrait)(getTraitId[FE](armodule, arTrait.descriptor))(ModulePaths.traitDef)(createTraitDefMessage)(
                module.GlobalDeclaration.Descriptor.TraitDescriptor(convertInNamespaceDescriptor(arTrait.descriptor))
              )(
                for {
                  instMethods <- emitF(fromComp(arTrait.methods))
                  instEntries <- processMethods(instMethods)

                  staticMethods <- emitF(fromComp(arTrait.staticMethods))
                  staticEntries <- processMethods(staticMethods)
                } yield instEntries ++ staticEntries
              )

            case GlobalBinding.GlobalClass(_, access, arClass) =>
              bindingCommon(access)(arClass)(getClassId[FE](armodule, arClass.descriptor))(ModulePaths.classDef)(createClassDefMessage)(
                module.GlobalDeclaration.Descriptor.ClassDescriptor(convertInNamespaceDescriptor(arClass.descriptor))
              )(
                for {
                  instMethods <- emitF(fromComp(arClass.methods))
                  instEntries <- processMethods(instMethods)

                  staticMethods <- emitF(fromComp(arClass.staticMethods))
                  staticEntries <- processMethods(staticMethods)

                  ctors <- emitF(fromComp(arClass.classConstructors))
                  ctorEntries <- processClassCtors(ctors)
                } yield instEntries ++ staticEntries ++ ctorEntries
              )

            case GlobalBinding.GlobalDataConstructor(_, access, dataCtor) =>
              bindingCommon(access)(dataCtor)(getDataCtorId[FE](armodule, dataCtor.descriptor))(ModulePaths.dataCtorDef)(createDataCtorDefMessage)(
                module.GlobalDeclaration.Descriptor.DataConstructorDescriptor(convertInNamespaceDescriptor(dataCtor.descriptor))
              )(
                for {
                  instMethods <- emitF(fromComp(dataCtor.methods))
                  instEntries <- processMethods(instMethods)
                } yield instEntries
              )

            case GlobalBinding.GlobalFunction(_, access, func) =>
              bindingCommon(access)(func)(getFuncId[FE](armodule, func.descriptor))(ModulePaths.funcDef)(createFuncDefMessage)(
                module.GlobalDeclaration.Descriptor.FunctionDescriptor(convertInNamespaceDescriptor(func.descriptor))
              )(
                StateT.pure[FE, ModuleIds, Vector[(String, GeneratedMessage)]](Vector.empty)
              )
          }

        def processMethods(methods: Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]): EmitF[FE, Vector[(String, GeneratedMessage)]] =
          methods.foldMapM { case MethodBinding(_, _, _, method) =>
            for {
              result <- fromEmit(createMethodDefMessage(armodule, method))
              _ <- addEmitId(result._1)
            } yield Vector(result)
          }

        def processClassCtors(ctors: Vector[ClassConstructorBinding[context.type, DeclarationPayloadSpecifier]]): EmitF[FE, Vector[(String, GeneratedMessage)]] =
          ctors.foldMapM { case ClassConstructorBinding(_, _, ctor) =>
            for {
              result <- fromEmit(createClassCtorDefMessage(armodule, ctor))
              _ <- addEmitId(result._1)
            } yield Vector(result)
          }



        private type RefEmitHandler = StateT[FE, (ModuleIds, ModuleRefs), Vector[(String, GeneratedMessage)]]

        private def lensState[A, B, C](s: StateT[FE, A, C])(lens: Lens[B, A]): StateT[FE, B, C] =
          StateT[FE, B, C] { b =>
            s.run(lens.get(b)).map { case (a, c) =>
              (lens.set(b)(a), c)
            }
          }

        private def emitRefsSimple[D1 <: Descriptor, D2, M <: GeneratedMessage]
        (refIds: ModuleIds => Map[D1, Int])
        (createPath: Int => String)
        (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => EmitF[FE, D2])
        (createRef: (Int, D2) => M)
        : RefEmitHandler =
          emitRefs(refIds)(createPath)(convertDescriptor)((moduleId, d2, _) => createRef(moduleId, d2))

        private def emitRefs[D1 <: Descriptor, ID, D2, M <: GeneratedMessage]
        (refIds: ModuleIds => Map[D1, ID])
        (createPath: ID => String)
        (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => EmitF[FE, D2])
        (createRef: (Int, D2, ID) => M)
        : RefEmitHandler =
          StateT.get[FE, (ModuleIds, ModuleRefs)].flatMap { case (moduleIds, _) =>
            refIds(moduleIds).toVector.traverse { case (desc, id) =>
                for {
                  moduleId <- lensState(getModuleId[FE](desc.moduleDescriptor))(lens[(ModuleIds, ModuleRefs)]._2)
                  _ <- StateT.modify[FE, (ModuleIds, ModuleRefs)] { case (moduleIds, moduleRefs) =>
                    (moduleIds, moduleRefs.copy(moduleRefIds = moduleRefs.moduleRefIds + (desc.moduleDescriptor -> moduleId)))
                  }

                  convDesc <- lensState(convertDescriptor(armodule, desc))(lens[(ModuleIds, ModuleRefs)]._1)
                  ref = createRef(moduleId, convDesc, id)

                } yield createPath(id) -> ref
              }
          }

        private def emitAllRefs
        (emitHandlers: RefEmitHandler*)
        : StateT[FE, ModuleIds, (ModuleRefs, Vector[Vector[(String, GeneratedMessage)]])] = {

          def impl(moduleRefs: ModuleRefs, acc: Vector[Vector[(String, GeneratedMessage)]]): StateT[FE, ModuleIds, (ModuleRefs, Vector[Vector[(String, GeneratedMessage)]])] =
            StateT[FE, ModuleIds, (ModuleRefs, Vector[Vector[(String, GeneratedMessage)]])](origState =>
              emitHandlers.toVector.sequence.run((origState, moduleRefs)).map {
                case ((moduleIds, moduleRefs), entries) =>
                  (moduleIds, (moduleRefs, entries))
              }
            )
              .flatMap { case (moduleRefs, entries) =>
                StateT.get[FE, ModuleIds].flatMap { newState =>
                  val newEntries = entries.flatten
                  if(newEntries.nonEmpty)
                    impl(moduleRefs, acc :+ newEntries)
                  else
                    (moduleRefs, acc).pure[StateT[FE, ModuleIds, ?]]
                }
              }

          impl(ModuleRefs(), Vector())
        }


      }

    def getObjId[F[_]: Monad, O, T, D, ID](value: T)(getDesc: T => D)(nextId: Lens[O, Int], idMap: Lens[O, Map[D, ID]])(createId: => StateT[F, O, Int => ID]): StateT[F, O, ID] =
      StateT.get[F, O].flatMap { ids =>
        val map = idMap.get(ids)
        val descriptor = getDesc(value)
        map.get(descriptor) match {
          case Some(id) => id.pure[StateT[F, O, ?]]
          case None =>
            for {
              createIdFunc <- createId
              ids <- StateT.get[F, O]
              idNum = nextId.get(ids)
              id = createIdFunc(idNum)
              _ <- StateT.set[F, O](
                idMap.set(nextId.set(ids)(idNum + 1))(map + (descriptor -> id))
              )
            } yield id
        }
      }

    def getObjIdInt[F[_]: Monad, O, D](descriptor: D)(nextId: Lens[O, Int], idMap: Lens[O, Map[D, Int]]): StateT[F, O, Int] =
      getObjId(descriptor)(identity)(nextId, idMap)((identity[Int] _).pure[StateT[F, O, ?]])

    def getModuleId[F[_]: Monad](descriptor: ModuleDescriptor): StateT[F, ModuleRefs, Int] =
      getObjIdInt(descriptor)(lens[ModuleRefs].nextModuleId, lens[ModuleRefs].moduleRefIds)

    def getTraitId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextTraitId, lens[ModuleIds].traitIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextTraitRefId, lens[ModuleIds].traitRefIds)(implicitly[Monad[F]]).map(-_)


    def getClassId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextClassId, lens[ModuleIds].classIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextClassRefId, lens[ModuleIds].classRefIds)(implicitly[Monad[F]]).map(-_)

    def getDataCtorId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextDataCtorId, lens[ModuleIds].dataCtorIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextDataCtorRefId, lens[ModuleIds].dataCtorRefIds)(implicitly[Monad[F]]).map(-_)

    def getFuncId[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): EmitF[F, Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextFunctionId, lens[ModuleIds].functionIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextFunctionRefId, lens[ModuleIds].functionRefIds)(implicitly[Monad[F]]).map(-_)

    def getMethodId[F[_]: Monad, TPayloadSpec[_, _]](armodule: ArModule[context.type, DeclarationPayloadSpecifier], method: ArMethod[context.type, TPayloadSpec])(implicit fromComp: TComp ~> F): EmitF[F, Int] =
      if(method.descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(method.descriptor)(lens[ModuleIds].nextMethodId, lens[ModuleIds].methodIds)
      else
        getObjId(method)(_.descriptor)(lens[ModuleIds].nextMethodRefId, lens[ModuleIds].methodRefIds)(
          method.owner match {
            case ArMethod.ClassOwner(ownerClass) =>
              getClassId[F](armodule, ownerClass.descriptor)
                .map { classId => id => (id, module.MethodReference.MethodOwner.OwnerClassId(classId)) }

            case ArMethod.ClassObjectOwner(ownerClass) =>
              getClassId[F](armodule, ownerClass.descriptor)
                .map { classId => id => (id, module.MethodReference.MethodOwner.OwnerClassObjectId(classId)) }

            case ArMethod.TraitOwner(ownerTrait) =>
              getTraitId[F](armodule, ownerTrait.descriptor)
                .map { traitId => id => (id, module.MethodReference.MethodOwner.OwnerTraitId(traitId)) }

            case ArMethod.TraitObjectOwner(ownerTrait) =>
              getTraitId[F](armodule, ownerTrait.descriptor)
                .map { traitId => id => (id, module.MethodReference.MethodOwner.OwnerTraitObjectId(traitId)) }

            case ArMethod.DataCtorOwner(dataCtor) =>
              getDataCtorId[F](armodule, dataCtor.descriptor)
                .map { ctorId => id => (id, module.MethodReference.MethodOwner.OwnerConstructorId(ctorId)) }
          }
        )(implicitly[Monad[F]]).map { case (id, _) => -id }

    def getClassCtorId[F[_]: Monad](descriptor: ClassConstructorDescriptor): EmitF[F, Int] =
      getObjIdInt(descriptor)(lens[ModuleIds].nextClassCtorId, lens[ModuleIds].classCtorIds)


    def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], A]
    (armodule: ArModule[context.type, DeclarationPayloadSpecifier])
    (sig: Signature[TResult])
    (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.type]) => Emit[A])
    : Emit[A] = {

      def impl(sig: Signature[TResult], prevParams: Vector[module.Parameter]): Emit[A] =
        sig.visit(
          sigParams =>
            sigParams.parameter.elements
              .traverse { elem =>
                convertType(armodule, elem.elemType).map { paramType =>
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
                impl(sigParams.nextUnsubstituted, prevParams :+ module.Parameter(elems))
              },
          sigResult => f(prevParams, sigResult.result),
        )

      impl(sig, Vector.empty)
    }

    def convertClassType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], classType: typeSystem.ClassType): Emit[module.ClassType] = for {
      id <- getClassId[TComp](armodule, classType.arClass.value.descriptor)
      args <- classType.args.traverse(convertTypeArg(armodule, _))
    } yield module.ClassType(id, args)

    def convertTraitType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], traitType: typeSystem.TraitType): Emit[module.TraitType] = for {
      id <- getTraitId[TComp](armodule, traitType.arTrait.value.descriptor)
      args <- traitType.args.traverse(convertTypeArg(armodule, _))
    } yield module.TraitType(id, args)

    def convertDataCtorType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], dataCtorType: typeSystem.DataConstructorType): Emit[module.DataConstructorType] = for {
      id <- getDataCtorId[TComp](armodule, dataCtorType.ctor.value.descriptor)
      args <- dataCtorType.args.traverse(convertTypeArg(armodule, _))
    } yield module.DataConstructorType(id, args)

    def convertType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], t: typeSystem.ArExpr): Emit[module.Type] =
      ((t match {
        case t: typeSystem.ClassType => convertClassType(armodule, t).map(module.Type.TypeInfo.ClassType)
        case t: typeSystem.TraitType => convertTraitType(armodule, t).map(module.Type.TypeInfo.TraitType)
        case t: typeSystem.DataConstructorType => convertDataCtorType(armodule, t).map(module.Type.TypeInfo.DataConstructorType)
        case _ => ???
      }) : Emit[module.Type.TypeInfo])
        .map(module.Type.apply)

    def convertTypeArg(armodule: ArModule[context.type, DeclarationPayloadSpecifier], t: typeSystem.TypeArgument): Emit[module.TypeArg] =
      t match {
        case typeSystem.TypeArgument.Expr(t) =>
          convertType(armodule, t)
            .map { modType => module.TypeArg(module.TypeArg.TypeInfo.Type(modType)) }

        case typeSystem.TypeArgument.Wildcard =>
          module.TypeArg(module.TypeArg.TypeInfo.Wildcard(module.Wildcard())).pure[Emit]
      }

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

    def convertFileId(id: FileID): module.FileID =
      module.FileID(id.id)

    def convertTraitDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): EmitF[F, module.TraitDescriptor] =
      module.TraitDescriptor(module.TraitDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ TraitDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[EmitF[F, ?]]

    def convertClassDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): EmitF[F, module.ClassDescriptor] =
      module.ClassDescriptor(module.ClassDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ ClassDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[EmitF[F, ?]]

    def convertDataCtorDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): EmitF[F, module.DataConstructorDescriptor] =
      module.DataConstructorDescriptor(module.DataConstructorDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[EmitF[F, ?]]

    def convertFuncDescriptor[F[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): EmitF[F, module.FunctionDescriptor] =
      module.FunctionDescriptor(module.FunctionDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ FuncDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[EmitF[F, ?]]

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

        instMethods <- createMethodMembers(armodule, arTrait.methods)
        staticMethods <- createMethodMembers(armodule, arTrait.staticMethods)

        convDesc <- convertTraitDescriptor[TComp](armodule, arTrait.descriptor)

      } yield module.TraitDefinition(
        descriptor = convDesc,
        fileId = convertFileId(arTrait.fileId),
        signature = convSig,
        isSealed = Some(arTrait.isSealed).filter(identity),
        methods = instMethods,
        staticMethods = staticMethods,
      )

    def createMethodMembers(armodule: ArModule[context.type, DeclarationPayloadSpecifier], methods: context.Comp[Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]]): Emit[Vector[module.MethodMember]] =
      emitComp(methods).flatMap {
        _.traverse { method =>
          for {
            convDesc <- convertMethodDescriptor[TComp](armodule, method.method.descriptor)
            methodId <- getMethodId[TComp, DeclarationPayloadSpecifier](armodule, method.method)
          } yield module.MethodMember(convDesc, methodId, convertAccessModifier(method.accessModifier))
        }
      }

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

        instMethods <- createMethodMembers(armodule, arClass.methods)
        staticMethods <- createMethodMembers(armodule, arClass.staticMethods)

        classCtors <- emitComp(arClass.classConstructors)
        ctors <- classCtors.traverse { ctor =>
          convertClassCtorDescriptor[TComp](armodule, ctor.ctor.descriptor).map { convDesc =>
            module.ClassConstructorMember(convDesc, ctor.index)
          }
        }

        convDesc <- convertClassDescriptor[TComp](armodule, arClass.descriptor)

      } yield module.ClassDefinition(
        descriptor = convDesc,
        fileId = convertFileId(arClass.fileId),
        signature = convSig,
        isOpen = Some(arClass.isOpen).filter(identity),
        isAbstract = Some(arClass.isAbstract).filter(identity),
        isSealed = Some(arClass.isSealed).filter(identity),
        fields = convFields,
        methods = instMethods,
        staticMethods = staticMethods,
        constructors = ctors
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

        methods <- createMethodMembers(armodule, dataCtor.methods)
        instMethods <- emitComp(dataCtor.methods)

        convDesc <- convertDataCtorDescriptor[TComp](armodule, dataCtor.descriptor)
        ctorDef = module.DataConstructorDefinition(
          descriptor = convDesc,
          fileId = convertFileId(dataCtor.fileId),
          signature = convSig,
          methods = methods,
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
        fileId = convertFileId(func.fileId),
        signature = convSig,
        effects = module.EffectInfo(
          isPure = func.effectInfo.isPure
        ),
      )

    def createMethodDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], method: ArMethod[context.type, DeclarationPayloadSpecifier]): Emit[(String, module.MethodDefinition)] =
      for {
        sig <- emitComp(method.signatureUnsubstituted)
        id <- getMethodId[TComp, DeclarationPayloadSpecifier](armodule, method)
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
          fileId = convertFileId(method.fileId),
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
        sig <- emitComp(ctor.signatureUnsubstituted)
        id <- getClassCtorId[TComp](ctor.descriptor)
        convSig <- convertSignature(armodule)(sig) { (params, _) =>
          module.ClassConstructorSignature(params).pure[Emit]
        }

        convDesc <- convertClassCtorDescriptor[TComp](armodule, ctor.descriptor)
        classCtorDef = module.ClassConstructorDefinition(
          descriptor = convDesc,
          fileId = convertFileId(ctor.fileId),
          signature = convSig,
          effects = module.EffectInfo(
            isPure = ctor.effectInfo.isPure
          ),
        )

      } yield ModulePaths.classCtorDef(id) -> classCtorDef


  }

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): ArStream[TCompRE, R, NonEmptyList[CompilationError], (String, GeneratedMessage)] =
    Implementation.moduleBindingStream[TCompRE, R, NonEmptyList[CompilationError]](ArStream.EffectConverter.id)(module)
        .transformWith(
          Implementation.moduleEmitTransform[TCompRE](ArStream.EffectConverter.id)(module)
            .into(StreamTransformation.flattenVector[TCompRE, R, NonEmptyList[CompilationError], (String, GeneratedMessage), Unit])
        )


}
