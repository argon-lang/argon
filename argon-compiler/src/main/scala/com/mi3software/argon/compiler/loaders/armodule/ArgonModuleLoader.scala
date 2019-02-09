package com.mi3software.argon.compiler.loaders.armodule

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.loaders.{ModuleLoader, NamespaceBuilder, StandardTypeLoaders}
import com.mi3software.argon.compiler.types._
import com.mi3software.argon.{module => ArgonModule}
import com.mi3software.argon.util._
import scalaz._
import Scalaz._

import scala.collection.immutable.{Map, Vector}

object ArgonModuleLoader {

  def apply(ctx: Context)(referencePayloadLoader: PayloadLoader[ctx.type, ReferencePayloadSpecifier]): ModuleLoader[ctx.type] = new ModuleLoader[ctx.type] {

    override type ModuleData = ArgonModule.Module

    override def loadResource[TComp[_] : Compilation, I](id: I)(implicit res: ResourceAccess[TComp, I]): TComp[Option[ArgonModule.Module]] =
      res.getExtension(id).flatMap {
        case "armodule" => res.loadThriftStruct(id)(ArgonModule.Module).map(Some.apply)
        case _ => Option.empty[ArgonModule.Module].point[TComp]
      }

    override def dataDescriptor(data: ArgonModule.Module): Option[ModuleDescriptor] =
      Some(ModuleDescriptor(data.name))

    override def dataReferencedModules(data: ArgonModule.Module): Vector[ModuleDescriptor] =
      data.referencedModules.map {
        case ArgonModule.ModuleReference(name) => ModuleDescriptor(name)
      }.toVector


    override def loadModuleReference
    (context: ctx.type)
    (data: ArgonModule.Module)
    (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
    : context.Comp[ArModule[context.type, ReferencePayloadSpecifier]] =
      loadModule(context)(data)(referencedModules)(referencePayloadLoader)

    private val currentFormatVersion = 1

    private trait ModuleCreator[TContext <: Context, Comp[_], TPayloadSpec[_, _]] {
      val module: Comp[ArModule[TContext, TPayloadSpec]]
    }

    private def loadModule[TPayloadSpec[_, _]]
    (context: ctx.type)
    (binModule: ArgonModule.Module)
    (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
    (payloadLoader: PayloadLoader[context.type, TPayloadSpec])
    : context.Comp[ArModule[context.type, TPayloadSpec]] = {

      val context2: context.type = context
      import context._, typeSystem.{ context => _, _ }, signatureContext.{ context => _, typeSystem => _, _ }

      def impl[TComp[+_] : Compilation](implicit compEv: LeibnizK[TComp, context.Comp]): TComp[ArModule[context.type, TPayloadSpec]] = {
        val currentModuleDescriptor = ModuleDescriptor(binModule.name)

        for {

          _ <-
            if(binModule.formatVersion === 0 || binModule.formatVersion > currentFormatVersion)
              implicitly[Compilation[TComp]].forErrors(CompilationError.UnsupportedModuleFormatVersion(binModule.formatVersion, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)))
            else
              implicitly[Monad[TComp]].point(())

          moduleCache <- Compilation[TComp].createCache[ArModule[context.type, TPayloadSpec]]

          module <- new ModuleCreator[context.type, TComp, TPayloadSpec] {


            private def parseNamespacePath(ns: ArgonModule.Namespace): NamespacePath =
              Option(ns) match {
                case Some(ArgonModule.Namespace(parts)) => NamespacePath(parts.toVector)
                case None => NamespacePath.empty
              }

            private object ValidGlobalName {
              def unapply(globalName: ArgonModule.GlobalName): Option[GlobalName] =
                globalName match {
                  case ArgonModule.GlobalName.NormalName(name) => Some(GlobalName.Normal(name))
                  case ArgonModule.GlobalName.Unnamed(ArgonModule.UnnamedGlobalName(fileID, index)) =>
                    Some(GlobalName.Unnamed)

                  case ArgonModule.GlobalName.UnknownUnionField(_) => None
                }
            }

            private object ParsedAccessModifier {
              def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifier] =
                accessModifier match {
                  case ArgonModule.AccessModifier.Invalid | ArgonModule.AccessModifier.EnumUnknownAccessModifier(_) => None
                  case ArgonModule.AccessModifier.Public => Some(AccessModifier.Public)
                  case ArgonModule.AccessModifier.Internal => Some(AccessModifier.Internal)
                  case ArgonModule.AccessModifier.Protected => Some(AccessModifier.Protected)
                  case ArgonModule.AccessModifier.ProtectedInternal => Some(AccessModifier.ProtectedInternal)
                  case ArgonModule.AccessModifier.Private => Some(AccessModifier.Private)
                  case ArgonModule.AccessModifier.PrivateInternal => Some(AccessModifier.PrivateInternal)
                }
            }

            private object ParsedGlobalAccessModifier {
              def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifierGlobal] =
                accessModifier match {
                  case ParsedAccessModifier(accessModifier: AccessModifierGlobal) => Some(accessModifier)
                  case _ => None
                }
            }



            private lazy val refModuleMap: Map[Int, ModuleLoadResult[context.type]] =
              binModule.referencedModules.zipWithIndex
                .map { case (modRef, i) =>
                  val moduleLoadRes: ModuleLoadResult[context.type] =
                    referencedModules
                      .find { _.descriptor.name.contains(modRef.name) }
                    match {
                      case Some(referencedModule) => ModuleReference(referencedModule)
                      case None => ModuleNotFound(modRef)
                    }

                  i -> moduleLoadRes
                }(collection.breakOut)

            private def lookupNamespaceValue[T]
            (refModule: ArModule[context.type, ReferencePayloadSpecifier])
            (namespace: NamespacePath, name: GlobalName)
            (f: PartialFunction[GlobalBinding[context.type, ReferencePayloadSpecifier], T])
            : TComp[Option[T]] =
              compEv.flip(ModuleLookup.lookupNamespaceValue(context)(refModule)(namespace, name)(f))

            private def parseTraitDescriptor(module: ModuleDescriptor)(desc: ArgonModule.TraitDescriptor): Option[TraitDescriptor] =
              desc match {
                case ArgonModule.TraitDescriptor.InNamespace(
                  ArgonModule.TraitDescriptorInNamespace(
                    ArgonModule.FileSpec(fileID, fileName),
                    ns,
                    ValidGlobalName(name),
                    ParsedGlobalAccessModifier(accessModifier)
                  )
                ) =>
                  Some(TraitDescriptor.InNamespace(module, FileID(fileID), 0, parseNamespacePath(ns), name, accessModifier))

                case ArgonModule.TraitDescriptor.UnknownUnionField(_) =>
                  None
              }

            private def parseClassDescriptor(module: ModuleDescriptor)(desc: ArgonModule.ClassDescriptor): Option[ClassDescriptor] =
              desc match {
                case ArgonModule.ClassDescriptor.InNamespace(
                  ArgonModule.ClassDescriptorInNamespace(
                    ArgonModule.FileSpec(fileID, fileName),
                    ns,
                    ValidGlobalName(name),
                    ParsedGlobalAccessModifier(accessModifier)
                  )
                ) =>
                  Some(ClassDescriptor.InNamespace(module, FileID(fileID), 0, parseNamespacePath(ns), name, accessModifier))

                case ArgonModule.ClassDescriptor.UnknownUnionField(_) =>
                  None
              }

            private def parseDataCtorDescriptor(module: ModuleDescriptor)(desc: ArgonModule.DataConstructorDescriptor): Option[DataConstructorDescriptor] =
              desc match {
                case ArgonModule.DataConstructorDescriptor.InNamespace(
                  ArgonModule.DataConstructorDecscriptorInNamespace(
                    ArgonModule.FileSpec(fileID, fileName),
                    ns,
                    ValidGlobalName(name),
                    ParsedGlobalAccessModifier(accessModifier)
                  )
                ) =>
                  Some(DataConstructorDescriptor.InNamespace(module, FileID(fileID), 0, parseNamespacePath(ns), name, accessModifier))

                case ArgonModule.DataConstructorDescriptor.UnknownUnionField(_) =>
                  None
              }

            private def parseFunctionDescriptor(module: ModuleDescriptor)(desc: ArgonModule.FunctionDescriptor): Option[FuncDescriptor] =
              desc match {
                case ArgonModule.FunctionDescriptor.InNamespace(
                  ArgonModule.FunctionDescriptorInNamespace(
                    ArgonModule.FileSpec(fileID, fileName),
                    ns,
                    ValidGlobalName(name),
                    ParsedGlobalAccessModifier(accessModifier)
                  )
                ) =>
                  Some(FuncDescriptor.InNamespace(module, FileID(fileID), 0, parseNamespacePath(ns), name, accessModifier))

                case ArgonModule.FunctionDescriptor.UnknownUnionField(_) =>
                  None
              }

            private def parseMemberName(memberName: ArgonModule.MemberName): Option[MethodName] =
              memberName match {

                case ArgonModule.MemberName.UnknownUnionField(_) => None

                case ArgonModule.MemberName.Name(name) =>
                  Some(MemberName.Normal(name))

                case ArgonModule.MemberName.UnnamedMemberIndex(index) =>
                  Some(MemberName.Unnamed)

                case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.EnumUnknownSpecialMemberName(_)) => None

                case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.Call) =>
                  Some(MemberName.Call)

                case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.New) =>
                  None

              }

            private def parseMethodDescriptor(module: ModuleDescriptor)(desc: ArgonModule.MethodDescriptor): Option[MethodDescriptor] =
              desc match {
                case ArgonModule.MethodDescriptor(memberName, ParsedGlobalAccessModifier(accessModifier), ownerDesc) =>
                  for {
                    memberNameValue <- parseMemberName(memberName)
                    ownerDescValue <- ownerDesc match {
                      case ArgonModule.MethodOwnerDescriptor.UnknownUnionField(_) => None
                      case ArgonModule.MethodOwnerDescriptor.TraitDescriptor(traitDesc) => parseTraitDescriptor(module)(traitDesc)
                      case ArgonModule.MethodOwnerDescriptor.ClassDescriptor(classDesc) => parseClassDescriptor(module)(classDesc)
                      case ArgonModule.MethodOwnerDescriptor.TraitObjectDescriptor(traitDesc) => parseTraitDescriptor(module)(traitDesc).map(TraitObjectDescriptor.apply)
                      case ArgonModule.MethodOwnerDescriptor.ClassObjectDescriptor(classDesc) => parseClassDescriptor(module)(classDesc).map(ClassObjectDescriptor.apply)
                    }
                  } yield MethodDescriptor(ownerDescValue, 0, memberNameValue, accessModifier)
              }

            private trait ModuleObjectRefDef[TValue, TRef, TDef] {
              def fromModuleObject(value: TValue): Option[TRef \/ TDef]
            }

            private def handleModuleObjectLoading
            [TValue, TRef, TDef, TValueDescriptor, TResultDescriptor, TRefResult, TDefResult]
            (
              moduleObjectType: CompilationError.ModuleObjectType,
            )(
              valueLens: ArgonModule.Module => Seq[TValue]
            )(
              refDef: ModuleObjectRefDef[TValue, TRef, TDef]
            )(
              refModuleIdLens: TRef => Int,
              refDescriptorLens: TRef => TValueDescriptor,
              defDescriptorLens: TDef => TValueDescriptor,
            )(
              parseDescriptor: ModuleDescriptor => TValueDescriptor => Option[TResultDescriptor]
            )(
              referenceHandler: ArModule[context.type, ReferencePayloadSpecifier] => TResultDescriptor => TComp[Option[TRefResult]],
              definitionHandler: Int => TDef => TResultDescriptor => TDefResult
            ): TComp[Map[Int, ModuleObjectLoadResult[TDefResult, TRefResult]]] =
              valueLens(binModule).toVector.zipWithIndex.traverse { case (moduleObject, i) =>
                ((refDef.fromModuleObject(moduleObject) match {
                  case Some(-\/(refValue)) =>
                    refModuleMap.get(refModuleIdLens(refValue)) match {
                      case Some(ModuleReference(moduleRef)) =>
                        parseDescriptor(moduleRef.descriptor)(refDescriptorLens(refValue)) match {
                          case Some(descriptor) =>
                            referenceHandler(moduleRef)(descriptor).flatMap {
                              case Some(refResult) => ModuleObjectReference(refResult).point[TComp]
                              case None => Compilation[TComp].forErrors(CompilationError.ModuleObjectNotFound(
                                moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                              ))
                            }

                          case None =>
                            Compilation[TComp].forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                              moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                            ))
                        }

                      case _ =>
                        Compilation[TComp].forErrors(CompilationError.ModuleObjectModuleNotLoaded(
                          moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        ))
                    }

                  case Some(\/-(defValue)) =>
                    parseDescriptor(currentModuleDescriptor)(defDescriptorLens(defValue)) match {
                      case Some(descriptor) =>
                        ModuleObjectDefinition(definitionHandler(i)(defValue)(descriptor)).point[TComp]

                      case None =>
                        Compilation[TComp].forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                          moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        ))
                    }

                  case None =>
                    Compilation[TComp].forErrors(CompilationError.ModuleObjectUndefined(
                      moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                    ))
                }) : TComp[ModuleObjectLoadResult[TDefResult, TRefResult]])
                  .map { i -> _ }
              }.map { elems => Map(elems: _*) }

            private def lookupTrait(module: ArModule[context.type, ReferencePayloadSpecifier]): TraitDescriptor => TComp[Option[ArTrait[context.type, ReferencePayloadSpecifier]]] = {
              case TraitDescriptor.InNamespace(_, _, _, namespace, name, _) =>
                lookupNamespaceValue(module)(namespace, name) {
                  case GlobalBinding.GlobalTrait(_, _, arTrait) => arTrait
                }
            }

            private lazy val traitMap: TComp[Map[Int, TraitLoadResult[context.type, TPayloadSpec]]] =
              handleModuleObjectLoading(
                CompilationError.ModuleObjectTrait
              )(
                valueLens = _.traits
              )(
                refDef = new ModuleObjectRefDef[ArgonModule.Trait, ArgonModule.TraitReference, ArgonModule.TraitDefinition] {
                  override def fromModuleObject(value: ArgonModule.Trait): Option[ArgonModule.TraitReference \/ ArgonModule.TraitDefinition] =
                    value match {
                      case ArgonModule.Trait.TraitRef(traitRef) => Some(-\/(traitRef))
                      case ArgonModule.Trait.TraitDef(traitDef) => Some(\/-(traitDef))
                      case ArgonModule.Trait.UnknownUnionField(_) => None
                    }
                }
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseTraitDescriptor
              )(
                referenceHandler = lookupTrait(_),
                definitionHandler = traitId => traitValue => traitDescriptor => new ArTrait[context.type, TPayloadSpec] {
                  override val context: context2.type = context2
                  override val contextProof: Leibniz[context.type, context.type, context.type, context.type] = Leibniz.refl

                  override val descriptor: TraitDescriptor = traitDescriptor

                  override val isSealed: Boolean = traitValue.isSealed

                  override lazy val signature: context.Comp[Signature[ArTrait.ResultInfo]] =
                    traitValue.signature match {
                      case ArgonModule.TraitSignature(parameters, baseTraits) =>
                        compEv(
                          for {
                            baseTraitsResolved <- baseTraits.toVector.traverse(resolveTraitType(_))
                            parametersResolved <- parameters.zipWithIndex.toVector.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[ArTrait.ResultInfo](
                              ArTrait.ResultInfo(typeSystem)(BaseTypeInfoTrait(baseTraitsResolved))
                            )

                          } yield parametersResolved.foldRight[Signature[ArTrait.ResultInfo]](result)(SignatureParameters[ArTrait.ResultInfo](_, _))
                        )
                    }

                  override lazy val methods: context.Comp[Vector[ArMethod[context.type, TPayloadSpec]]] =
                    compEv(
                      methodMap.map { methods =>
                        methods.collect {
                          case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                            case ownerDesc: TraitDescriptor => ownerDesc === descriptor
                            case _ => false
                          }) =>
                            method
                        }.toVector
                      }
                    )

                  override val staticMethods: context.Comp[Vector[ArMethod[context.type, TPayloadSpec]]] =
                    compEv(
                      methodMap.map { methods =>
                        methods.collect {
                          case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                            case TraitObjectDescriptor(ownerDesc) => ownerDesc === descriptor
                            case _ => false
                          }) =>
                            method
                        }.toVector
                      }
                    )

                  override lazy val payload: TPayloadSpec[Unit, context.TTraitMetadata] = payloadLoader.createTraitPayload(context)
                }
              )

            private def lookupClass(module: ArModule[context.type, ReferencePayloadSpecifier]): ClassDescriptor => TComp[Option[ArClass[context.type, ReferencePayloadSpecifier]]] = {
              case ClassDescriptor.InNamespace(_, _, _, namespace, name, _) =>
                lookupNamespaceValue(module)(namespace, name)(ModuleLookup.lookupGlobalClass)
            }

            private lazy val classMap: TComp[Map[Int, ClassLoadResult[context.type, TPayloadSpec]]] =
              handleModuleObjectLoading(
                CompilationError.ModuleObjectClass
              )(
                valueLens = _.classes
              )(
                refDef = new ModuleObjectRefDef[ArgonModule.Class, ArgonModule.ClassReference, ArgonModule.ClassDefinition] {
                  override def fromModuleObject(value: ArgonModule.Class): Option[ArgonModule.ClassReference \/ ArgonModule.ClassDefinition] =
                    value match {
                      case ArgonModule.Class.ClassRef(classRef) => Some(-\/(classRef))
                      case ArgonModule.Class.ClassDef(classDef) => Some(\/-(classDef))
                      case ArgonModule.Class.UnknownUnionField(_) => None
                    }
                }
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseClassDescriptor
              )(
                referenceHandler = lookupClass(_),
                definitionHandler = classId => classValue => classDescriptor => new ArClass[context.type, TPayloadSpec] {
                  override val context: context2.type = context2
                  override val contextProof: Leibniz[context.type, context.type, context.type, context.type] = Leibniz.refl

                  override val descriptor: ClassDescriptor = classDescriptor

                  override val isSealed: Boolean = classValue.isSealed
                  override val isOpen: Boolean = classValue.isOpen
                  override val isAbstract: Boolean = classValue.isAbstract

                  override lazy val signature: context.Comp[Signature[ArClass.ResultInfo]] =
                    classValue.signature match {
                      case ArgonModule.ClassSignature(parameters, baseClass, baseTraits) =>
                        compEv(
                          for {
                            baseClassResolved <- baseClass.traverse(resolveClassType(_))
                            baseTraitsResolved <- baseTraits.toVector.traverse(resolveTraitType(_))
                            parametersResolved <- parameters.zipWithIndex.toVector.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[ArClass.ResultInfo](
                              ArClass.ResultInfo(typeSystem)(BaseTypeInfoClass(baseClassResolved, baseTraitsResolved))
                            )

                          } yield parametersResolved.foldRight[Signature[ArClass.ResultInfo]](result)(SignatureParameters[ArClass.ResultInfo](_, _))
                        )
                    }

                  override val fields: context.Comp[Vector[context.typeSystem.Variable[FieldDescriptor]]] =
                    compEv(
                      classValue.fields.toVector.traverse { field =>
                        for {
                          fieldType <- resolveType(field.fieldType)
                        } yield Variable(
                          FieldDescriptor(descriptor, field.name),
                          VariableName.Normal(field.name),
                          Mutability.fromIsMutable(field.isMutable),
                          fieldType
                        )
                      }
                    )

                  override lazy val methods: context.Comp[Vector[ArMethod[context.type, TPayloadSpec]]] =
                    compEv(
                      methodMap.map { methods =>
                        methods.collect {
                          case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                            case ownerDesc: ClassDescriptor => ownerDesc === descriptor
                            case _ => false
                          }) =>
                            method
                        }.toVector
                      }
                    )

                  override lazy val staticMethods: context.Comp[Vector[ArMethod[context.type, TPayloadSpec]]] =
                    compEv(
                      methodMap.map { methods =>
                        methods.collect {
                          case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                            case ClassObjectDescriptor(ownerDesc) => ownerDesc === descriptor
                            case _ => false
                          }) =>
                            method
                        }.toVector
                      }
                    )

                  override lazy val payload: TPayloadSpec[Unit, context.TClassMetadata] = payloadLoader.createClassPayload(context)

                  override val classConstructors: context.Comp[Vector[ClassConstructor[context.type, TPayloadSpec]]] =
                    context.compCompilationInstance.point(Vector.empty)

                }
              )

            private lazy val dataCtorMap: TComp[Map[Int, DataCtorLoadResult[context.type, TPayloadSpec]]] =
              handleModuleObjectLoading(
                CompilationError.ModuleObjectDataConstructor
              )(
                valueLens = _.dataConstructors
              )(
                refDef = new ModuleObjectRefDef[ArgonModule.DataConstructor, ArgonModule.DataConstructorReference, ArgonModule.DataConstructorDefinition] {
                  override def fromModuleObject(value: ArgonModule.DataConstructor): Option[ArgonModule.DataConstructorReference \/ ArgonModule.DataConstructorDefinition] =
                    value match {
                      case ArgonModule.DataConstructor.DataCtorRef(dataCtorRef) => Some(-\/(dataCtorRef))
                      case ArgonModule.DataConstructor.DataCtorDef(dataCtorDef) => Some(\/-(dataCtorDef))
                      case ArgonModule.DataConstructor.UnknownUnionField(_) => None
                    }
                }
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseDataCtorDescriptor
              )(
                referenceHandler = moduleRef => {
                  case DataConstructorDescriptor.InNamespace(_, _, _, namespace, name, _) =>
                    lookupNamespaceValue(moduleRef)(namespace, name) {
                      case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => dataCtor
                    }
                },
                definitionHandler = _ => ???
              )

            private lazy val functionMap: TComp[Map[Int, FunctionLoadResult[context.type, TPayloadSpec]]] =
              handleModuleObjectLoading(
                CompilationError.ModuleObjectFunction
              )(
                valueLens = _.functions
              )(
                refDef = new ModuleObjectRefDef[ArgonModule.Function, ArgonModule.FunctionReference, ArgonModule.FunctionDefinition] {
                  override def fromModuleObject(value: ArgonModule.Function): Option[ArgonModule.FunctionReference \/ ArgonModule.FunctionDefinition] =
                    value match {
                      case ArgonModule.Function.FuncRef(funcRef) => Some(-\/(funcRef))
                      case ArgonModule.Function.FuncDef(funcDef) => Some(\/-(funcDef))
                      case ArgonModule.Function.UnknownUnionField(_) => None
                    }
                }
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseFunctionDescriptor
              )(
                referenceHandler = moduleRef => {
                  case FuncDescriptor.InNamespace(_, _, _, namespace, name, _) =>
                    lookupNamespaceValue(moduleRef)(namespace, name) {
                      case GlobalBinding.GlobalFunction(_, _, func) => func
                    }
                },
                definitionHandler = funcId => funcValue => funcDescriptor => new ArFunc[context.type, TPayloadSpec] {
                  override val context: ctx.type = ctx
                  override val descriptor: FuncDescriptor = funcDescriptor
                  override val effectInfo: EffectInfo = EffectInfo(
                    isPure = funcValue.effects.isPure,
                  )
                  override val signature: this.context.Comp[Signature[FunctionResultInfo]] =
                    funcValue.signature match {
                      case ArgonModule.FunctionSignature(parameters, returnType) =>
                        compEv(
                          for {
                            returnTypeResolved <- resolveType(returnType)
                            parametersResolved <- parameters.zipWithIndex.toVector.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[FunctionResultInfo](
                              FunctionResultInfo(typeSystem)(returnTypeResolved)
                            )

                          } yield parametersResolved.foldRight[Signature[FunctionResultInfo]](result)(SignatureParameters[FunctionResultInfo](_, _))
                        )
                    }

                  override val payload: TPayloadSpec[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata] =
                    payloadLoader.createFunctionPayload(context)
                }
              )

            private lazy val methodMap: TComp[Map[Int, MethodLoadResult[context.type, TPayloadSpec]]] =
              handleModuleObjectLoading(
                CompilationError.ModuleObjectMethod
              )(
                valueLens = _.methods
              )(
                refDef = new ModuleObjectRefDef[ArgonModule.Method, ArgonModule.MethodReference, ArgonModule.MethodDefinition] {
                  override def fromModuleObject(value: ArgonModule.Method): Option[ArgonModule.MethodReference \/ ArgonModule.MethodDefinition] =
                    value match {
                      case ArgonModule.Method.MethodRef(methodRef) => Some(-\/(methodRef))
                      case ArgonModule.Method.MethodDef(methodDef) => Some(\/-(methodDef))
                      case ArgonModule.Method.UnknownUnionField(_) => None
                    }
                }
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseMethodDescriptor
              )(
                referenceHandler = moduleRef => {
                  case methodDesc @ MethodDescriptor(traitDescriptor: TraitDescriptor, _, _, _) =>
                    lookupTrait(moduleRef)(traitDescriptor).flatMap { arTraitOpt =>
                      arTraitOpt.traverseM[TComp, ArMethod[context.type, ReferencePayloadSpecifier]] { arTrait =>
                        compEv.flip(arTrait.methods).map { methods =>
                          methods.find { method =>
                            method.descriptor === methodDesc
                          }
                        }
                      }
                    }
                  case methodDesc @ MethodDescriptor(TraitObjectDescriptor(traitDescriptor), _, _, _) =>
                    lookupTrait(moduleRef)(traitDescriptor).flatMap { arTraitOpt =>
                      arTraitOpt.traverseM[TComp, ArMethod[context.type, ReferencePayloadSpecifier]] { arTrait =>
                        compEv.flip(arTrait.staticMethods).map { methods =>
                          methods.find { method =>
                            method.descriptor === methodDesc
                          }
                        }
                      }
                    }

                  case methodDesc @ MethodDescriptor(classDescriptor : ClassDescriptor, _, _, _) =>
                    lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                      arClassOpt.traverseM[TComp, ArMethod[context.type, ReferencePayloadSpecifier]] { arClass =>
                        compEv.flip(arClass.methods).map { methods =>
                          methods.find { method =>
                            method.descriptor === methodDesc
                          }
                        }
                      }
                    }

                  case methodDesc @ MethodDescriptor(ClassObjectDescriptor(classDescriptor), _, _, _) =>
                    lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                      arClassOpt.traverseM[TComp, ArMethod[context.type, ReferencePayloadSpecifier]] { arClass =>
                        compEv.flip(arClass.staticMethods).map { methods =>
                          methods.find { method =>
                            method.descriptor === methodDesc
                          }
                        }
                      }
                    }

                  case methodDesc @ MethodDescriptor(DataConstructorDescriptor.InNamespace(_, _, _, namespace, name, _), _, _, _) =>
                    lookupNamespaceValue(moduleRef)(namespace, name) {
                      case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => dataCtor
                    }
                      .flatMap {
                        _.traverseM { dataCtor =>
                          compEv.flip(dataCtor.methods).map { methods =>
                            methods.find { method =>
                              method.descriptor === methodDesc
                            }
                          }
                      }
                    }
                },
                definitionHandler = methodId => methodValue => methodDescriptor => new ArMethod[context.type, TPayloadSpec] {
                  override val context: ctx.type = ctx
                  override val contextProof: Leibniz[context.type, context.type, context.type, context.type] = Leibniz.refl
                  override val descriptor: MethodDescriptor = methodDescriptor
                  override val effectInfo: EffectInfo = EffectInfo(
                    isPure = methodValue.effects.isPure,
                  )

                  override val isVirtual: Boolean = methodValue.isVirtual
                  override val isAbstract: Boolean = methodValue.isAbstract
                  override val isImplicitOverride: Boolean = methodValue.isImplicitOverride
                  override val isFinal: Boolean = methodValue.isFinal

                  override val signature: context.Comp[Signature[FunctionResultInfo]] =
                    methodValue.signature match {
                      case ArgonModule.MethodSignature(parameters, returnType) =>
                        compEv(
                          for {
                            returnTypeResolved <- resolveType(returnType)
                            parametersResolved <- parameters.zipWithIndex.toVector.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[FunctionResultInfo](
                              FunctionResultInfo(typeSystem)(returnTypeResolved)
                            )

                          } yield parametersResolved.foldRight[Signature[FunctionResultInfo]](result)(SignatureParameters[FunctionResultInfo](_, _))
                        )
                    }

                  override val payload: TPayloadSpec[context.Comp[context.TMethodImplementation], context.TMethodMetadata] =
                    payloadLoader.createMethodPayload(context)
                }
              )

            private def createNamespaceElements[TRef, TDef, TResult]
            (
              elementMap: TComp[Map[Int, ModuleObjectLoadResult[TDef, TRef]]]
            )(
              f: TDef => Option[ModuleElement[context.type, TPayloadSpec]]
            ): TComp[Vector[ModuleElement[context.type, TPayloadSpec]]] =
              elementMap.map {
                _.toVector.flatMap {
                  case (_, ModuleObjectDefinition(element)) => f(element).toList.toVector
                  case (_, ModuleObjectReference(_)) => Vector.empty
                }
              }

            private def combineNamespaceElements(elements: TComp[Vector[ModuleElement[context.type, TPayloadSpec]]]*): TComp[Vector[ModuleElement[context.type, TPayloadSpec]]] =
              elements.toVector.sequence.map { _.flatten }

            private lazy val globalNamespaceComp: TComp[Namespace[context.type, TPayloadSpec]] =
              combineNamespaceElements(
                createNamespaceElements(traitMap) { arTrait =>
                  arTrait.descriptor match {
                    case TraitDescriptor.InNamespace(_, _, _, namespace, name, accessModifier) =>
                      Some(ModuleElement(namespace, GlobalBinding.GlobalTrait(name, accessModifier, arTrait)))
                  }
                },
                createNamespaceElements(classMap) { arClass =>
                  arClass.descriptor match {
                    case ClassDescriptor.InNamespace(_, _, _, namespace, name, accessModifier) =>
                      Some(ModuleElement(namespace, GlobalBinding.GlobalClass(name, accessModifier, arClass)))
                  }
                },
                createNamespaceElements(dataCtorMap) { dataCtor =>
                  dataCtor.descriptor match {
                    case DataConstructorDescriptor.InNamespace(_, _, _, namespace, name, accessModifier) =>
                      Some(ModuleElement(namespace, GlobalBinding.GlobalDataConstructor(name, accessModifier, dataCtor)))
                  }
                },
                createNamespaceElements(functionMap) { func =>
                  func.descriptor match {
                    case FuncDescriptor.InNamespace(_, _, _, namespace, name, accessModifier) =>
                      Some(ModuleElement(namespace, GlobalBinding.GlobalFunction(name, accessModifier, func)))
                  }
                },
              ).map(NamespaceBuilder.createNamespace)


            def findTrait(traitId: Int): TComp[AbsRef[context.type, ArTrait]] =
              traitMap.flatMap { _.get(traitId) match {
                case Some(ModuleObjectReference(arTrait)) => AbsRef(arTrait).point[TComp]
                case Some(ModuleObjectDefinition(arTrait)) => AbsRef(arTrait).point[TComp]
                case None =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

              } }


            def findTraitDef(traitId: Int): TComp[ArTrait[context.type, TPayloadSpec]] =
              traitMap.flatMap { _.get(traitId) match {
                case Some(ModuleObjectDefinition(arTrait)) => arTrait.point[TComp]


                case Some(ModuleObjectReference(_)) =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

                case None =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

              } }


            def findClass(classId: Int): TComp[AbsRef[context.type, ArClass]] =
              classMap.flatMap { _.get(classId) match {
                case Some(ModuleObjectReference(arClass)) => AbsRef(arClass).point[TComp]
                case Some(ModuleObjectDefinition(arClass)) => AbsRef(arClass).point[TComp]
                case None =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

              } }


            def findClassDef(classId: Int): TComp[ArClass[context.type, TPayloadSpec]] =
              classMap.flatMap { _.get(classId) match {
                case Some(ModuleObjectDefinition(arClass)) => arClass.point[TComp]


                case Some(ModuleObjectReference(_)) =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

                case None =>
                  implicitly[Compilation[TComp]].forErrors(
                    CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                  )

              } }

            def resolveSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], T]
            (sig: Signature[TResult], args: Vector[ArgonModule.Type], convArgs: Vector[TType])
            (f: (Vector[TType], TResult[context.type, context.typeSystem.type]) => T)
            : TComp[T] =
              sig.visit(
                sigParams => args match {
                  case head +: tail =>
                    resolveType(head).flatMap { argType =>
                      sigParams.next[TComp](argType).flatMap(resolveSignature(_, tail, convArgs :+ argType)(f))
                    }
                  case _ => ???
                },
                sigResult => if(args.nonEmpty) ??? else f(convArgs, sigResult.result).point[TComp]
              )

            def resolveTraitType(traitType: ArgonModule.TraitType): TComp[TraitType] =
              findTrait(traitType.traitId).flatMap { arTrait =>
                compEv.flip(arTrait.value.signature).flatMap { sig =>
                  resolveSignature(sig, traitType.typeArguments.toVector, Vector.empty) {
                    (args, result) => TraitType(arTrait, args, result.baseTypes)
                  }
                }
              }

            def resolveClassType(classType: ArgonModule.ClassType): TComp[ClassType] =
              findClass(classType.classId).flatMap { arClass =>
                compEv.flip(arClass.value.signature).flatMap { sig =>
                  resolveSignature(sig, classType.typeArguments.toVector, Vector.empty) {
                    (args, result) => ClassType(arClass, args, result.baseTypes)
                  }
                }
              }

            def resolveType(t: ArgonModule.Type): TComp[TType] =
              t match {
                case ArgonModule.Type.TraitType(traitType) => resolveTraitType(traitType)
                case ArgonModule.Type.ClassType(classType) => resolveClassType(classType)
                case ArgonModule.Type.ConstructorInstanceType(constructorInstanceType) => ???
                case ArgonModule.Type.UnknownUnionField(_) => ???
              }

            def resolveParameter(ownerDescriptor: ParameterOwnerDescriptor)(index: Int)(parameter: ArgonModule.Parameter): TComp[Parameter] =
              parameter.elements
                .zipWithIndex
                .toVector
                .traverse { case (ArgonModule.ParameterElement(name, paramType), tupleIndex) =>
                  resolveType(paramType)
                    .map(Variable(DeconstructedParameterDescriptor(ownerDescriptor, index, tupleIndex), VariableName.Normal(name), Mutability.Mutable, _))
                }
                .flatMap {
                  case Vector() =>
                    for {
                      currentModule <- module
                      unitType <- compEv.flip(
                        StandardTypeLoaders.loadUnitType(context)(
                          CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        )(currentModule)(referencedModules)
                      )

                    } yield Parameter(Vector(), unitType)




                  case variables @ head +: tail =>

                  val paramType = context.typeSystem.fromSimpleType(context.typeSystem.LoadTupleType(
                    NonEmptyList(head, tail: _*).map { v => context.typeSystem.TupleElement(v.varType) }
                  ))

                  Parameter(variables, paramType).point[TComp]
                }

            override lazy val module: TComp[ArModule[context.type, TPayloadSpec]] =
              moduleCache(
                for {
                  globalNamespaceCache <- Compilation[TComp].createCache[Namespace[context.type, TPayloadSpec]]
                } yield new ArModule[context.type, TPayloadSpec] {
                  override val context: context2.type = context2
                  override val descriptor: ModuleDescriptor = currentModuleDescriptor
                  override lazy val globalNamespace: context.Comp[Namespace[context.type, TPayloadSpec]] = compEv(globalNamespaceCache(globalNamespaceComp))
                  override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] =
                    refModuleMap.values.collect {
                      case ModuleReference(moduleRef) => moduleRef
                    }(collection.breakOut)
                }
              )

          }.module

        } yield module
      }

      impl[context.Comp](context.compCompilationInstance, LeibnizK.refl)
    }

  }




  trait PayloadLoader[TContext <: Context with Singleton, TPayloadSpec[_, _]] {

    def createClassPayload(context: TContext): TPayloadSpec[Unit, context.TClassMetadata]
    def createTraitPayload(context: TContext): TPayloadSpec[Unit, context.TTraitMetadata]
    def createDataConstructorPayload(context: TContext): TPayloadSpec[context.Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata]
    def createFunctionPayload(context: TContext): TPayloadSpec[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata]
    def createMethodPayload(context: TContext): TPayloadSpec[context.Comp[context.TMethodImplementation], context.TMethodMetadata]
    def createClassConstructorPayload(context: TContext): TPayloadSpec[context.Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata]

  }

  sealed trait ModuleLoadResult[TContext <: Context]
  final case class ModuleReference[TContext <: Context](module: ArModule[TContext, ReferencePayloadSpecifier]) extends ModuleLoadResult[TContext]
  final case class ModuleNotFound[TContext <: Context](moduleRef: ArgonModule.ModuleReference) extends ModuleLoadResult[TContext]

  sealed trait ModuleObjectLoadResult[+TDefinition, +TReference]
  final case class ModuleObjectReference[+TDefinition, +TReference](value: TReference) extends ModuleObjectLoadResult[TDefinition, TReference]
  final case class ModuleObjectDefinition[+TDefinition, +TReference](value: TDefinition) extends ModuleObjectLoadResult[TDefinition, TReference]

  type TraitLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArTrait[TContext, PayloadSpec], ArTrait[TContext, ReferencePayloadSpecifier]]
  type ClassLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArClass[TContext, PayloadSpec], ArClass[TContext, ReferencePayloadSpecifier]]
  type DataCtorLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[DataConstructor[TContext, PayloadSpec], DataConstructor[TContext, ReferencePayloadSpecifier]]
  type FunctionLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArFunc[TContext, PayloadSpec], ArFunc[TContext, ReferencePayloadSpecifier]]
  type MethodLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArMethod[TContext, PayloadSpec], ArMethod[TContext, ReferencePayloadSpecifier]]

}
