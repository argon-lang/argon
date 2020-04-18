package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats._
import cats.data._
import cats.evidence.===
import cats.implicits._
import dev.argon.compiler.loaders.armodule.{ModuleFormatVersion, ModulePaths}
import dev.argon.compiler.types.{ParameterStyle, TypeSystem}
import dev.argon.util.FileID
import dev.argon.module
import dev.argon.stream.builder.{GenEffect, Sink, Source}
import dev.argon.stream.Resource
import shapeless._
import dev.argon.util.AnyExtensions._
import shapeless.ops.nat.{LT, Pred}
import zio.{IO, ZIO, stream => zstream}

final class ModuleEmitter[TComp[+_], TContext <: ModuleContext[TComp, _] with Singleton](val context: TContext) {

  import context._
  import context.signatureContext.{ Signature, SignatureResult, SignatureParameters, SignatureVisitor }

  type StreamElem = (String, GeneratedMessage)

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

    def processModule[G[_]: Monad](armodule: ArModule[context.type, DeclarationPayloadSpecifier])(sink: Sink[G, StreamElem])(implicit genEffect: GenEffect[Emit, G]): G[Unit] = {

      def addEmitId(id: String): Emit[Unit] =
        StateT.modify[Comp, ModuleIds](s => s.copy(emittedPaths = s.emittedPaths + id))

      def addGlobalDecl(g: module.GlobalDeclaration): Emit[Unit] =
        StateT.modify[Comp, ModuleIds](s => s.copy(globals = s.globals :+ g))

      def bindingCommon[V, M <: GeneratedMessage]
      (access: AccessModifierGlobal)
      (value: V)
      (getId: Emit[Int])
      (getPath: Int => String)
      (createMessage: (ArModule[context.type, DeclarationPayloadSpecifier], V) => Emit[M])
      (descriptor: module.GlobalDeclaration.Descriptor)
      (emitExtra: G[Unit])
      : G[Unit] =
        genEffect.liftF(getId).flatMap { id =>
          val path = getPath(id)

          genEffect.liftF(StateT.get[Comp, ModuleIds]).flatMap { state =>
            if(state.emittedPaths.contains(path))
              ().pure[G]
            else
              for {
                result <- genEffect.liftF(createMessage(armodule, value))
                _ <- genEffect.liftF(addEmitId(path))
                _ <- sink.consume(path -> result)
                _ <- emitExtra

                _ <- genEffect.liftF(addGlobalDecl(module.GlobalDeclaration(descriptor, id, convertAccessModifier(access))))

              } yield ()
          }
        }

      def processNamespace(ns: Namespace[context.type, DeclarationPayloadSpecifier]): G[Unit] =
        ns.bindings.traverse_ {
          case GlobalBinding.NestedNamespace(_, nestedNS) => processNamespace(nestedNS)
          case GlobalBinding.GlobalTrait(_, access, arTrait) =>
            bindingCommon(access)(arTrait)(getTraitId(armodule, arTrait.descriptor))(ModulePaths.traitDef)(createTraitDefMessage)(
              module.GlobalDeclaration.Descriptor.TraitDescriptor(convertInNamespaceDescriptor(arTrait.descriptor))
            )(
              for {
                instMethods <- genEffect.liftF(emitComp(arTrait.methods))
                _ <- processMethods(instMethods)

                staticMethods <- genEffect.liftF(emitComp(arTrait.staticMethods))
                _ <- processMethods(staticMethods)
              } yield ()
            )

          case GlobalBinding.GlobalClass(_, access, arClass) =>
            bindingCommon(access)(arClass)(getClassId(armodule, arClass.descriptor))(ModulePaths.classDef)(createClassDefMessage)(
              module.GlobalDeclaration.Descriptor.ClassDescriptor(convertInNamespaceDescriptor(arClass.descriptor))
            )(
              for {
                instMethods <- genEffect.liftF(emitComp(arClass.methods))
                _ <- processMethods(instMethods)

                staticMethods <- genEffect.liftF(emitComp(arClass.staticMethods))
                _ <- processMethods(staticMethods)

                ctors <- genEffect.liftF(emitComp(arClass.classConstructors))
                _ <- processClassCtors(ctors)
              } yield ()
            )

          case GlobalBinding.GlobalDataConstructor(_, access, dataCtor) =>
            bindingCommon(access)(dataCtor)(getDataCtorId(armodule, dataCtor.descriptor))(ModulePaths.dataCtorDef)(createDataCtorDefMessage)(
              module.GlobalDeclaration.Descriptor.DataConstructorDescriptor(convertInNamespaceDescriptor(dataCtor.descriptor))
            )(
              for {
                instMethods <- genEffect.liftF(emitComp(dataCtor.methods))
                instEntries <- processMethods(instMethods)
              } yield instEntries
            )

          case GlobalBinding.GlobalFunction(_, access, func) =>
            bindingCommon(access)(func)(getFuncId(armodule, func.descriptor))(ModulePaths.funcDef)(createFuncDefMessage)(
              module.GlobalDeclaration.Descriptor.FunctionDescriptor(convertInNamespaceDescriptor(func.descriptor))
            )(
              ().pure[G]
            )

        }

      def processMethods(methods: Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]): G[Unit] =
        methods.traverse_ {
          case MethodBinding(_, _, _, method) =>
            for {
              result <- genEffect.liftF(createMethodDefMessage(armodule, method))
              _ <- genEffect.liftF(addEmitId(result._1))
              _ <- sink.consume(result)
            } yield ()
        }

      def processClassCtors(ctors: Vector[ClassConstructorBinding[context.type, DeclarationPayloadSpecifier]]): G[Unit] =
        ctors.traverse_ {
          case ClassConstructorBinding(_, _, ctor) =>
            for {
              result <- genEffect.liftF(createClassCtorDefMessage(armodule, ctor))
              _ <- genEffect.liftF(addEmitId(result._1))
              _ <- sink.consume(result)
            } yield ()
        }

      def emitRefsSimple[D1 <: Descriptor, D2, M <: GeneratedMessage]
      (refIds: ModuleIds => Map[D1, Int])
      (createPath: Int => String)
      (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => Emit[D2])
      (createRef: (Int, D2, scalapb.UnknownFieldSet) => M)
      : ModuleRefs => G[(ModuleRefs, Boolean)] =
        emitRefs(refIds)(createPath)(convertDescriptor)((moduleId, d2, _) => createRef(moduleId, d2, scalapb.UnknownFieldSet.empty))

      def emitRefs[D1 <: Descriptor, ID, D2, M <: GeneratedMessage]
      (refIds: ModuleIds => Map[D1, ID])
      (createPath: ID => String)
      (convertDescriptor: (ArModule[context.type, DeclarationPayloadSpecifier], D1) => Emit[D2])
      (createRef: (Int, D2, ID) => M)
      : ModuleRefs => G[(ModuleRefs, Boolean)] = moduleRefs =>
        genEffect.liftF(StateT.get[TComp, ModuleIds]).flatMap { moduleIds =>
          refIds(moduleIds).toVector.foldLeftM((moduleRefs, false)) {
            case ((moduleRefs, hasEmitted), (desc, id)) =>
              val path = createPath(id)

              if(moduleIds.emittedPaths.contains(path))
                (moduleRefs, hasEmitted).pure[G]
              else
                for {
                  _ <- genEffect.liftF(addEmitId(path))
                  (moduleRefs, moduleId) <- genEffect.liftF(emitComp(getModuleId(desc.moduleDescriptor).run(moduleRefs)))

                  convDesc <- genEffect.liftF(convertDescriptor(armodule, desc))
                  _ <- sink.consume(path -> createRef(moduleId, convDesc, id))
                } yield (moduleRefs.copy(moduleRefIds = moduleRefs.moduleRefIds + (desc.moduleDescriptor -> moduleId)), true)
          }
        }


      def processReferences: G[Unit] =
        for {
          moduleRefs <- emitAllRefs(
            emitRefs(_.methodRefIds)({ case (id, _) => ModulePaths.methodRef(id) })(convertMethodDescriptor) {
              case (moduleId, desc, (_, owner)) =>
                module.MethodReference(moduleId, desc, owner)
            },
            emitRefsSimple(_.classCtorRefIds)(ModulePaths.classCtorRef)(convertClassCtorDescriptor)(module.ClassConstructorReference.apply),
            emitRefsSimple(_.traitRefIds)(ModulePaths.traitRef)(convertTraitDescriptor)(module.TraitReference.apply),
            emitRefsSimple(_.classRefIds)(ModulePaths.classRef)(convertClassDescriptor)(module.ClassReference.apply),
            emitRefsSimple(_.dataCtorRefIds)(ModulePaths.dataCtorRef)(convertDataCtorDescriptor)(module.DataConstructorReference.apply),
            emitRefsSimple(_.functionRefIds)(ModulePaths.funcRef)(convertFuncDescriptor)(module.FunctionReference.apply),
          )

          moduleIds <- genEffect.liftF(StateT.get[TComp, ModuleIds])
          _ <- sink.consume(ModulePaths.metadata -> module.Metadata(
            formatVersion = ModuleFormatVersion.currentVersion,
            name = armodule.descriptor.name,
            references =
            moduleRefs.moduleRefIds
              .toVector
              .sortBy { case (_, id) => id }
              .map { case (ModuleDescriptor(name), _) => module.ModuleReference(name) },
            globals = moduleIds.globals
          ))
        } yield ()

      def emitAllRefs
      (emitHandlers: (ModuleRefs => G[(ModuleRefs, Boolean)])*)
      : G[ModuleRefs] = {

        def impl(moduleRefs: ModuleRefs): G[ModuleRefs] =
          emitHandlers.toVector.foldLeftM((moduleRefs, false)) {
            case ((moduleRefs, hasEmitted), emitRefs) =>
              emitRefs(moduleRefs).map { case (moduleRefs, hasEmittedPart) => (moduleRefs, hasEmitted || hasEmittedPart) }
          }
            .flatMap {
              case (moduleRefs, true) => impl(moduleRefs)
              case (moduleRefs, false) => moduleRefs.pure[G]
            }

        impl(ModuleRefs())
      }


      genEffect.liftF(emitComp(armodule.globalNamespace))
        .flatMap(processNamespace(_))
        .flatMap { _ => processReferences }
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

    def getObjIdInt[O, D](descriptor: D)(nextId: Lens[O, Int], idMap: Lens[O, Map[D, Int]]): StateT[TComp, O, Int] =
      getObjId(descriptor)(identity)(nextId, idMap)((identity[Int] _).pure[StateT[TComp, O, ?]])

    def getModuleId(descriptor: ModuleDescriptor): StateT[TComp, ModuleRefs, Int] =
      getObjIdInt(descriptor)(lens[ModuleRefs].nextModuleId, lens[ModuleRefs].moduleRefIds)

    def getTraitId(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): Emit[Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextTraitId, lens[ModuleIds].traitIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextTraitRefId, lens[ModuleIds].traitRefIds).map(-_)


    def getClassId(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): Emit[Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextClassId, lens[ModuleIds].classIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextClassRefId, lens[ModuleIds].classRefIds).map(-_)

    def getDataCtorId(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): Emit[Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextDataCtorId, lens[ModuleIds].dataCtorIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextDataCtorRefId, lens[ModuleIds].dataCtorRefIds).map(-_)

    def getFuncId(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): Emit[Int] =
      if(descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(descriptor)(lens[ModuleIds].nextFunctionId, lens[ModuleIds].functionIds)
      else
        getObjIdInt(descriptor)(lens[ModuleIds].nextFunctionRefId, lens[ModuleIds].functionRefIds).map(-_)

    def getMethodId[TPayloadSpec[_, _]](armodule: ArModule[context.type, DeclarationPayloadSpecifier], method: ArMethod[context.type, TPayloadSpec]): Emit[Int] =
      if(method.descriptor.moduleDescriptor === armodule.descriptor)
        getObjIdInt(method.descriptor)(lens[ModuleIds].nextMethodId, lens[ModuleIds].methodIds)
      else
        getObjId(method)(_.descriptor)(lens[ModuleIds].nextMethodRefId, lens[ModuleIds].methodRefIds)(
          method.owner match {
            case ArMethod.ClassOwner(ownerClass) =>
              getClassId(armodule, ownerClass.descriptor)
                .map { classId => id => (id, module.MethodReference.MethodOwner.OwnerClassId(classId)) }

            case ArMethod.ClassObjectOwner(ownerClass) =>
              getClassId(armodule, ownerClass.descriptor)
                .map { classId => id => (id, module.MethodReference.MethodOwner.OwnerClassObjectId(classId)) }

            case ArMethod.TraitOwner(ownerTrait) =>
              getTraitId(armodule, ownerTrait.descriptor)
                .map { traitId => id => (id, module.MethodReference.MethodOwner.OwnerTraitId(traitId)) }

            case ArMethod.TraitObjectOwner(ownerTrait) =>
              getTraitId(armodule, ownerTrait.descriptor)
                .map { traitId => id => (id, module.MethodReference.MethodOwner.OwnerTraitObjectId(traitId)) }

            case ArMethod.DataCtorOwner(dataCtor) =>
              getDataCtorId(armodule, dataCtor.descriptor)
                .map { ctorId => id => (id, module.MethodReference.MethodOwner.OwnerConstructorId(ctorId)) }
          }
        ).map { case (id, _) => -id }

    def getClassCtorId(descriptor: ClassConstructorDescriptor): Emit[Int] =
      getObjIdInt(descriptor)(lens[ModuleIds].nextClassCtorId, lens[ModuleIds].classCtorIds)


    def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], A]
    (armodule: ArModule[context.type, DeclarationPayloadSpecifier])
    (sig: Signature[TResult, _ <: Nat])
    (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.type]) => Emit[A])
    : Emit[A] = {

      def impl[Len <: Nat](sig: Signature[TResult, Len], prevParams: Vector[module.Parameter]): Emit[A] =
        sig.visit(new SignatureVisitor[TResult, Len, Emit[A]] {
          override def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Emit[A] =
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
                val style = sigParams.parameter.style match {
                  case ParameterStyle.Normal => None
                  case ParameterStyle.Inferrable => Some(module.ParameterStyle.Inferrable)
                }

                impl(sigParams.nextUnsubstituted, prevParams :+ module.Parameter(style, elems))
              }

          override def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): Emit[A] =
            f(prevParams, sigResult.result)

        })

      impl(sig, Vector.empty)
    }

    def convertClassType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], classType: typeSystem.ClassType): Emit[module.ClassType] = for {
      id <- getClassId(armodule, classType.arClass.value.descriptor)
      args <- classType.args.traverse(convertTypeArg(armodule, _))
    } yield module.ClassType(id, args)

    def convertTraitType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], traitType: typeSystem.TraitType): Emit[module.TraitType] = for {
      id <- getTraitId(armodule, traitType.arTrait.value.descriptor)
      args <- traitType.args.traverse(convertTypeArg(armodule, _))
    } yield module.TraitType(id, args)

    def convertDataCtorType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], dataCtorType: typeSystem.DataConstructorType): Emit[module.DataConstructorType] = for {
      id <- getDataCtorId(armodule, dataCtorType.ctor.value.descriptor)
      args <- dataCtorType.args.traverse(convertTypeArg(armodule, _))
    } yield module.DataConstructorType(id, args)

    def convertType(armodule: ArModule[context.type, DeclarationPayloadSpecifier], t: typeSystem.ArExpr): Emit[module.Type] =
      ((t match {
        case t: typeSystem.ClassType => convertClassType(armodule, t).map(module.Type.TypeInfo.ClassType)
        case t: typeSystem.TraitType => convertTraitType(armodule, t).map(module.Type.TypeInfo.TraitType)
        case t: typeSystem.DataConstructorType => convertDataCtorType(armodule, t).map(module.Type.TypeInfo.DataConstructorType)
        case _ => ???
      }) : Emit[module.Type.TypeInfo])
        .map(module.Type(_))

    def convertTypeArg(armodule: ArModule[context.type, DeclarationPayloadSpecifier], t: typeSystem.TypeArgument): Emit[module.TypeArg] =
      t match {
        case typeSystem.TypeArgument.Expr(t) =>
          convertType(armodule, t)
            .map { modType => module.TypeArg(module.TypeArg.TypeInfo.Type(modType)) }

        case typeSystem.TypeArgument.Wildcard(_) =>
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

    def convertTraitDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: TraitDescriptor): Emit[module.TraitDescriptor] =
      module.TraitDescriptor(module.TraitDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ TraitDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[Emit]

    def convertClassDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassDescriptor): Emit[module.ClassDescriptor] =
      module.ClassDescriptor(module.ClassDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ ClassDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[Emit]

    def convertDataCtorDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: DataConstructorDescriptor): Emit[module.DataConstructorDescriptor] =
      module.DataConstructorDescriptor(module.DataConstructorDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[Emit]

    def convertFuncDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: FuncDescriptor): Emit[module.FunctionDescriptor] =
      module.FunctionDescriptor(module.FunctionDescriptor.Descriptor.InNamespace(
        descriptor match {
          case descriptor @ FuncDescriptor.InNamespace(_, _, _, _) => convertInNamespaceDescriptor(descriptor)
        }
      )).pure[Emit]

    def convertMethodDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: MethodDescriptor): Emit[module.MethodDescriptor] =
      for {
        (instanceType, parentId) <- descriptor.typeDescriptor match {
          case descriptor: TraitDescriptor =>
            for {
              desc <- convertTraitDescriptor(armodule, descriptor)
              parentId <- getTraitId(armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.TraitDescriptor(desc), parentId)

          case TraitObjectDescriptor(descriptor) =>
            for {
              desc <- convertTraitDescriptor(armodule, descriptor)
              parentId <- getTraitId(armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.TraitObjectDescriptor(desc), parentId)

          case descriptor: ClassDescriptor =>
            for {
              desc <- convertClassDescriptor(armodule, descriptor)
              parentId <- getClassId(armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.ClassDescriptor(desc), parentId)

          case ClassObjectDescriptor(descriptor) =>
            for {
              desc <- convertClassDescriptor(armodule, descriptor)
              parentId <- getClassId(armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.ClassObjectDescriptor(desc), parentId)

          case descriptor: DataConstructorDescriptor =>
            for {
              desc <- convertDataCtorDescriptor(armodule, descriptor)
              parentId <- getDataCtorId(armodule, descriptor)
            } yield (module.MethodDescriptor.InstanceType.DataConstructorDescriptor(desc), parentId)
        }
      } yield module.MethodDescriptor(
        index = descriptor.index,
        instanceTypeId = parentId,
        memberName = descriptor.name match {
          case MemberName.Normal(name) => module.MethodDescriptor.MemberName.Name(name)
          case MemberName.Mutator(name) => module.MethodDescriptor.MemberName.Mutator(name)
          case MemberName.Unnamed => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Unnamed)
          case MemberName.Call => module.MethodDescriptor.MemberName.SpecialMethodName(module.SpecialMethodName.Call)
        },
        instanceType = instanceType
      )

    def convertClassCtorDescriptor(armodule: ArModule[context.type, DeclarationPayloadSpecifier], descriptor: ClassConstructorDescriptor): Emit[module.ClassConstructorDescriptor] =
      for {
        ownerClass <- convertClassDescriptor(armodule, descriptor.ownerClass)
      } yield module.ClassConstructorDescriptor(
        ownerClass = ownerClass,
        index = descriptor.index,
        instanceClassId = descriptor.ownerClass match {
          case ClassDescriptor.InNamespace(_, id, _, _) => id.toInt
        },
      )

    def createTraitDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Emit[module.TraitDefinition] =
      for {
        sig <- emitComp(arTrait.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            baseTypes <- emitComp(result.baseTypes)
            baseTraits <- baseTypes.baseTraits.traverse(convertTraitType(armodule, _))
          } yield module.TraitSignature(
            parameters = params,
            baseTraits = baseTraits,
          )
        }

        instMethods <- createMethodMembers(armodule, arTrait.methods)
        staticMethods <- createMethodMembers(armodule, arTrait.staticMethods)

        convDesc <- convertTraitDescriptor(armodule, arTrait.descriptor)

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
            convDesc <- convertMethodDescriptor(armodule, method.method.descriptor)
            methodId <- getMethodId(armodule, method.method)
          } yield module.MethodMember(convDesc, methodId, convertAccessModifier(method.accessModifier))
        }
      }

    def createClassDefMessage(armodule: ArModule[context.type, DeclarationPayloadSpecifier], arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Emit[module.ClassDefinition] =
      for {
        sig <- emitComp(arClass.signature)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            baseTypes <- emitComp(result.baseTypes)
            baseClass <- baseTypes.baseClass.traverse(convertClassType(armodule, _))
            baseTraits <- baseTypes.baseTraits.traverse(convertTraitType(armodule, _))
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
          convertClassCtorDescriptor(armodule, ctor.ctor.descriptor).map { convDesc =>
            module.ClassConstructorMember(convDesc, ctor.index, convertAccessModifier(ctor.accessModifier))
          }
        }

        convDesc <- convertClassDescriptor(armodule, arClass.descriptor)

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

        convDesc <- convertDataCtorDescriptor(armodule, dataCtor.descriptor)
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

        convDesc <- convertFuncDescriptor(armodule, func.descriptor)

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
        id <- getMethodId(armodule, method)
        convSig <- convertSignature(armodule)(sig) { (params, result) =>
          for {
            returnType <- convertType(armodule, result.returnType)
          } yield module.MethodSignature(
            parameters = params,
            returnType = returnType,
          )
        }

        methodOwner <- method.owner match {
          case ArMethod.ClassOwner(ownerClass) => getClassId(armodule, ownerClass.descriptor).map(module.MethodDefinition.MethodOwner.OwnerClassId.apply)
          case ArMethod.ClassObjectOwner(ownerClass) => getClassId(armodule, ownerClass.descriptor).map(module.MethodDefinition.MethodOwner.OwnerClassObjectId.apply)
          case ArMethod.TraitOwner(ownerTrait) => getTraitId(armodule, ownerTrait.descriptor).map(module.MethodDefinition.MethodOwner.OwnerTraitId.apply)
          case ArMethod.TraitObjectOwner(ownerTrait) => getTraitId(armodule, ownerTrait.descriptor).map(module.MethodDefinition.MethodOwner.OwnerTraitObjectId.apply)
          case ArMethod.DataCtorOwner(dataCtor) => getDataCtorId(armodule, dataCtor.descriptor).map(module.MethodDefinition.MethodOwner.OwnerConstructorId.apply)
        }

        convDesc <- convertMethodDescriptor(armodule, method.descriptor)
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
        id <- getClassCtorId(ctor.descriptor)
        convSig <- convertSignature(armodule)(sig) { (params, _) =>
          module.ClassConstructorSignature(params).pure[Emit]
        }

        convDesc <- convertClassCtorDescriptor(armodule, ctor.descriptor)
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

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): Source[TComp, StreamElem, Unit] =
    new Source[TComp, StreamElem, Unit] {

      import Implementation.{processModule, Emit, emitComp, ModuleIds}


      override protected val monadF: Monad[TComp] = implicitly

      override protected def generateImpl[G[_] : Monad](sink: Sink[G, StreamElem])(implicit genEffect: GenEffect[TComp, G]): G[Unit] = {
        import GenEffect._

        processModule(module)(new Sink[StateT[G, ModuleIds, *], StreamElem] {
          override def consume(value: StreamElem): StateT[G, Implementation.ModuleIds, Unit] =
            StateT.liftF(sink.consume(value))
        })
          .runA(ModuleIds())
      }

    }


}
