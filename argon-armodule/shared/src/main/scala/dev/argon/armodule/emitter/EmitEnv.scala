package dev.argon.armodule.emitter

import shapeless.Id
import dev.argon.armodule.emitter.DeclRefIDPair
import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.core._
import dev.argon.compiler.expr.LocalVariable
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.util.NamespacePath
import zio.{IO, Ref, UIO}


private[emitter] trait EmitEnv[TContext <: Context with Singleton] {

  val options: ModuleEmitOptions
  val vtableBuilder: VTableBuilder.Aux[TContext]

  def getModuleIdNum[TPayloadSpec[_, _]: PayloadSpecInfo](module: ArModule[TContext, TPayloadSpec]): Comp[Option[Int]]

  def getNamespaceIdNum(ns: NamespacePath): Comp[Int]
  def getNamespaceForID(id: Int): Comp[NamespacePath]

  val classes: DeclRefIDPair[TContext, ArClass]
  val traits: DeclRefIDPair[TContext, ArTrait]
  val dataConstructors: DeclRefIDPair[TContext, DataConstructor]
  val functions: DeclRefIDPair[TContext, ArFunc]
  val methods: DeclRefIDPair[TContext, ArMethod]
  val classConstructors: DeclRefIDPair[TContext, ClassConstructor]

  def getLocalVariableIdNum[TPayloadSpec[_, _]](variable: LocalVariable[TContext, Id]): Comp[Int]

}

private[emitter] object EmitEnv {

  def make(context: Context)(emitOptions: ModuleEmitOptions)(referencedModules: Vector[ModuleId]): UIO[EmitEnv[context.type]] =
    for {
      vtableBuilderObj <- VTableBuilder(context)

      namespaceIds <- Ref.make(IdentifierState.initial[NamespacePath, NamespacePath])

      classIds <- Ref.make(IdentifierState.initial[ClassId, ArClass[context.type, DeclarationPayloadSpecifier]])
      traitIds <- Ref.make(IdentifierState.initial[TraitId, ArTrait[context.type, DeclarationPayloadSpecifier]])
      dataCtorIds <- Ref.make(IdentifierState.initial[DataConstructorId, DataConstructor[context.type, DeclarationPayloadSpecifier]])
      functionIds <- Ref.make(IdentifierState.initial[FunctionId, ArFunc[context.type, DeclarationPayloadSpecifier]])
      methodIds <- Ref.make(IdentifierState.initial[MethodId, ArMethod[context.type, DeclarationPayloadSpecifier]])
      classCtorIds <- Ref.make(IdentifierState.initial[ClassConstructorId, ClassConstructor[context.type, DeclarationPayloadSpecifier]])

      classRefIds <- Ref.make(IdentifierState.initial[ClassId, ArClass[context.type, ReferencePayloadSpecifier]])
      traitRefIds <- Ref.make(IdentifierState.initial[TraitId, ArTrait[context.type, ReferencePayloadSpecifier]])
      dataCtorRefIds <- Ref.make(IdentifierState.initial[DataConstructorId, DataConstructor[context.type, ReferencePayloadSpecifier]])
      functionRefIds <- Ref.make(IdentifierState.initial[FunctionId, ArFunc[context.type, ReferencePayloadSpecifier]])
      methodRefIds <- Ref.make(IdentifierState.initial[MethodId, ArMethod[context.type, ReferencePayloadSpecifier]])
      classCtorRefIds <- Ref.make(IdentifierState.initial[ClassConstructorId, ClassConstructor[context.type, ReferencePayloadSpecifier]])

      localVarIds <- Ref.make(IdentifierState.initial[LocalVariableId, Unit])

    } yield (new EmitEnv[context.type] {

      abstract class IDPairImpl[ID, TElem[_ <: Context with Singleton, _[_, _]]]
      (
        declIds: Ref[IdentifierState[ID, TElem[context.type, DeclarationPayloadSpecifier]]],
        refIds: Ref[IdentifierState[ID, TElem[context.type, ReferencePayloadSpecifier]]],
      ) extends DeclRefIDPair[context.type, TElem] {

        def getID[TPayloadSpec[_, _]](elem: TElem[context.type, TPayloadSpec]): ID

        override def getIdNum[TPayloadSpec[_, _] : PayloadSpecInfo](elem: TElem[context.type, TPayloadSpec]): Comp[Int] =
          implicitly[PayloadSpecInfo[TPayloadSpec]].visit(elem)(new PayloadSpecVisitor[TElem[context.type, *[_, _]], Comp[Int]] {
            override def visitDeclaration(container: TElem[context.type, DeclarationPayloadSpecifier]): Comp[Int] =
              IdentifierState.getIdNum(declIds)(getID(container), container)

            override def visitReference(container: TElem[context.type, ReferencePayloadSpecifier]): Comp[Int] =
              IdentifierState.getIdNum(refIds)(getID(container), container).map { -_ }
          })

        override def getByID(id: Int): Comp[AbsRef[context.type, TElem]] =
          if(id > 0) declIds.get.map { idState => AbsRef(idState.elems(id - 1)) }
          else if(id < 0) refIds.get.map { idState => AbsRef(idState.elems(-(id + 1))) }
          else IO.die(new RuntimeException("0 is not a valid id"))

        override def getDeclaration(id: Int): Comp[TElem[context.type, DeclarationPayloadSpecifier]] =
          declIds.get.map { idState => idState.elems(id - 1) }

        override def getReference(id: Int): Comp[TElem[context.type, ReferencePayloadSpecifier]] =
          refIds.get.map { idState => idState.elems(id - 1) }
      }

      override val options: ModuleEmitOptions = emitOptions

      override val vtableBuilder: VTableBuilder.Aux[context.type] = vtableBuilderObj


      override def getModuleIdNum[TPayloadSpec[_, _] : PayloadSpecInfo](module: ArModule[context.type, TPayloadSpec]): Comp[Option[Int]] =
        IO.succeed(
          implicitly[PayloadSpecInfo[TPayloadSpec]].visit(module)(new PayloadSpecVisitor[ArModule[context.type, *[_, _]], Option[Int]] {
            override def visitDeclaration(container: ArModule[context.type, DeclarationPayloadSpecifier]): Option[Int] = None

            override def visitReference(container: ArModule[context.type, ReferencePayloadSpecifier]): Option[Int] = {
              val index = referencedModules.indexOf(module.id)
              if(index < 0) ???
              else Some(index + 1)
            }

          })
        )

      override def getNamespaceIdNum(ns: NamespacePath): Comp[Int] =
        IdentifierState.getIdNum(namespaceIds)(ns, ns)

      override def getNamespaceForID(id: Int): Comp[NamespacePath] =
        namespaceIds.get.map(_.elems(id - 1))

      override val classes: DeclRefIDPair[context.type, ArClass] = new IDPairImpl[ClassId, ArClass](classIds, classRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: ArClass[context.type, TPayloadSpec]): ClassId = elem.id
      }

      override val traits: DeclRefIDPair[context.type, ArTrait] = new IDPairImpl[TraitId, ArTrait](traitIds, traitRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: ArTrait[context.type, TPayloadSpec]): TraitId = elem.id
      }

      override val dataConstructors: DeclRefIDPair[context.type, DataConstructor] = new IDPairImpl[DataConstructorId, DataConstructor](dataCtorIds, dataCtorRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: DataConstructor[context.type, TPayloadSpec]): DataConstructorId = elem.id
      }

      override val functions: DeclRefIDPair[context.type, ArFunc] = new IDPairImpl[FunctionId, ArFunc](functionIds, functionRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: ArFunc[context.type, TPayloadSpec]): FunctionId = elem.id
      }

      override val methods: DeclRefIDPair[context.type, ArMethod] = new IDPairImpl[MethodId, ArMethod](methodIds, methodRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: ArMethod[context.type, TPayloadSpec]): MethodId = elem.id
      }

      override val classConstructors: DeclRefIDPair[context.type, ClassConstructor] = new IDPairImpl[ClassConstructorId, ClassConstructor](classCtorIds, classCtorRefIds) {
        override def getID[TPayloadSpec[_, _]](elem: ClassConstructor[context.type, TPayloadSpec]): ClassConstructorId = elem.id
      }

      override def getLocalVariableIdNum[TPayloadSpec[_, _]](variable: LocalVariable[context.type, Id]): Comp[Int] =
        IdentifierState.getIdNum(localVarIds)(variable.id, ())



    } : EmitEnv[context.type])



}
