package dev.argon.armodule.emitter

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.module.ModuleFormatVersion
import zio._
import zio.stream._
import dev.argon.{module => ArgonModule}
import dev.argon.armodule.errors.ImplementationIsNotExtern
import dev.argon.compiler.output.ArgonModuleSerialized

object ModuleSerializer {
  def serialize(context: Context)(emitOptions: ModuleEmitOptions)(module: ArModule[context.type, DeclarationPayloadSpecifier]): UIO[ArgonModuleSerialized] = for {
    emitEnv <- EmitEnv.make(context)(emitOptions)(module.referencedModules.map(_.id))
  } yield (new ArgonModuleSerialized {

    private val moduleSerializerElements = ModuleSerializerElements(context)(emitEnv)
    import moduleSerializerElements.{context => _, _}

    override val metadata: ArgonModule.Metadata =
      ArgonModule.Metadata(
        formatVersion = ModuleFormatVersion.currentVersion,
        name = module.id.name,
        moduleType = emitOptions.moduleType match {
          case ModuleEmitOptions.ReferenceModule => ArgonModule.ModuleType.DefinitionModule
          case ModuleEmitOptions.DeclarationModule => ArgonModule.ModuleType.InterfaceModule
        }
      )

    override def references: Comp[ArgonModule.ModuleReferencesList] =
      IO.succeed(
        ArgonModule.ModuleReferencesList(
          module.referencedModules.map { refMod =>
            ArgonModule.ModuleReference(name = refMod.id.name)
          }
        )
      )

    override def namespaces: CompStream[ArgonModule.NamespaceDeclaration] =
      module.namespaces.mapM { ns =>
        for {
          nsID <- emitEnv.getNamespaceIdNum(ns)
        } yield ArgonModule.NamespaceDeclaration(convertNamespace(ns), nsID)
      }

    override def namespaceElements(id: Int): CompStream[ArgonModule.GlobalDeclarationElement] =
      ZStream.unwrap(emitEnv.getNamespaceForID(id).map(module.getNamespace(_)))
        .mapM {
          case GlobalBinding.GlobalClass(name, access, sig, arClass) =>
            for {
              arClassValue <- arClass
              id <- emitEnv.classes.getIdNum(arClassValue)

              convSig <- convertErasedSignatureParameterOnly(sig, arClassValue.signature.map(ErasedSignature.fromSignatureParameters(context)(_)))
            } yield ArgonModule.GlobalDeclarationElement.ClassElement(ArgonModule.GlobalDeclarationType(id, convertGlobalName(name),convertAccessModifier(access), convSig))

          case GlobalBinding.GlobalTrait(name, access, sig, arTrait) =>
            for {
              arTraitValue <- arTrait
              id <- emitEnv.traits.getIdNum(arTraitValue)

              convSig <- convertErasedSignatureParameterOnly(sig, arTraitValue.signature.map(ErasedSignature.fromSignatureParameters(context)(_)))
            } yield ArgonModule.GlobalDeclarationElement.TraitElement(ArgonModule.GlobalDeclarationType(id, convertGlobalName(name),convertAccessModifier(access), convSig))

          case GlobalBinding.GlobalFunction(name, access, sig, func) =>
            for {
              funcValue <- func
              id <- emitEnv.functions.getIdNum(funcValue)

              convSig <- convertErasedSignature(sig, funcValue.signature.map(ErasedSignature.fromSignature(context)(_)))
            } yield ArgonModule.GlobalDeclarationElement.FunctionElement(ArgonModule.GlobalDeclarationFunction(id, convertGlobalName(name),convertAccessModifier(access), convSig))

          case GlobalBinding.GlobalDataConstructor(name, access, sig, dataCtor) =>
            for {
              ctorValue <- dataCtor
              id <- emitEnv.dataConstructors.getIdNum(ctorValue)

              convSig <- convertErasedSignatureParameterOnly(sig, ctorValue.signature.map(ErasedSignature.fromSignatureParameters(context)(_)))
            } yield ArgonModule.GlobalDeclarationElement.DataConstructorElement(ArgonModule.GlobalDeclarationType(id, convertGlobalName(name),convertAccessModifier(access), convSig))
        }

    override def getTraitDef(id: Int): Comp[ArgonModule.TraitDefinition] =
      emitEnv.traits.getDeclaration(id).flatMap(createTraitDefMessage)

    override def getTraitRef(id: Int): Comp[ArgonModule.TraitReference] =
      emitEnv.traits.getReference(id).flatMap(createTraitRefMessage)

    override def getClassDef(id: Int): Comp[ArgonModule.ClassDefinition] =
      emitEnv.classes.getDeclaration(id).flatMap(createClassDefMessage)

    override def getClassRef(id: Int): Comp[ArgonModule.ClassReference] =
      emitEnv.classes.getReference(id).flatMap(createClassRefMessage)

    override def getDataConstructorDef(id: Int): Comp[ArgonModule.DataConstructorDefinition] =
      emitEnv.dataConstructors.getDeclaration(id).flatMap(createDataCtorDefMessage)

    override def getDataConstructorRef(id: Int): Comp[ArgonModule.DataConstructorReference] =
      emitEnv.dataConstructors.getReference(id).flatMap(createDataCtorRefMessage)

    override def getFunctionDef(id: Int): Comp[ArgonModule.FunctionDefinition] =
      emitEnv.functions.getDeclaration(id).flatMap(createFuncDefMessage)

    override def getFunctionRef(id: Int): Comp[ArgonModule.FunctionReference] =
      emitEnv.functions.getReference(id).flatMap(createFuncRefMessage)

    override def getFunctionPayload(id: Int, platformId: String): CompStream[Byte] =
      ZStream.unwrap(
        emitEnv.functions.getDeclaration(id)
          .flatMap { func => func.payload }
          .map {
            case impl: FunctionImplementation.Extern[context.externHandler.ExternFunction] =>
              context.externHandler.encodeExternalFunction(impl.source, impl.extern)(platformId)

            case FunctionImplementation.Expression(_) => Stream.die(new ImplementationIsNotExtern())
          }
      )

    override def getMethodDef(id: Int): Comp[ArgonModule.MethodDefinition] =
      emitEnv.methods.getDeclaration(id).flatMap(createMethodDefMessage)

    override def getMethodRef(id: Int): Comp[ArgonModule.MethodReference] =
      emitEnv.methods.getReference(id).flatMap(createMethodRefMessage)

    override def getMethodPayload(id: Int, platformId: String): CompStream[Byte] =
      ZStream.unwrap(
        emitEnv.methods.getDeclaration(id)
          .flatMap { method => method.payload }
          .map {
            case impl: MethodImplementation.Extern[context.externHandler.ExternMethod] =>
              context.externHandler.encodeExternalMethod(impl.source, impl.extern)(platformId)

            case MethodImplementation.Expression(_) | MethodImplementation.Abstract => Stream.die(new ImplementationIsNotExtern())
          }
      )

    override def getClassConstructorDef(id: Int): Comp[ArgonModule.ClassConstructorDefinition] =
      emitEnv.classConstructors.getDeclaration(id).flatMap(createClassCtorDefMessage)

    override def getClassConstructorRef(id: Int): Comp[ArgonModule.ClassConstructorReference] =
      emitEnv.classConstructors.getReference(id).flatMap(createClassCtorRefMessage)

    override def getClassConstructorPayload(id: Int, platformId: String): CompStream[Byte] =
      Stream.die(new ImplementationIsNotExtern())

  } : ArgonModuleSerialized)







}
