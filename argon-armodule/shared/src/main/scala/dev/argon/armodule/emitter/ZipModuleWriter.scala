package dev.argon.armodule.emitter

import dev.argon.armodule.ModulePaths
import dev.argon.compiler.output.ArgonModuleSerialized
import dev.argon.compiler.{Comp, CompStream, Compilation, CompilationError, RComp, RCompStream}
import dev.argon.io.{ZipCreator, ZipEntryInfo}
import dev.argon.module._
import dev.argon.stream.builder.Source
import dev.argon.util.{MaybeBlocking, ProtoBufCodecs}
import scalapb.GeneratedMessage
import zio.{IO, Ref, ZIO}
import zio.stream.ZStream

private[emitter] abstract class ZipModuleWriter[R <: MaybeBlocking] {
  val module: ArgonModuleSerialized

  def emitPath(path: String, data: RCompStream[R, Byte]): RComp[R, Boolean]
  def emitPath(path: String, message: GeneratedMessage): RComp[R, Boolean] =
    emitPath(path, ProtoBufCodecs.serializeProtocolBuffer(message).catchAll(Compilation.unwrapThrowableStream))

  def emitPath(path: String, message: Comp[GeneratedMessage]): RComp[R, Boolean] = message.flatMap(emitPath(path, _))
  def emitPathAnd(path: String, message: GeneratedMessage)(ifEmitted: RComp[R, Unit]): RComp[R, Unit] =
    emitPath(path, message).flatMap { emitted =>
      if(emitted) ifEmitted
      else IO.unit
    }

  def emitPathAnd(path: String, message: Comp[GeneratedMessage])(ifEmitted: RComp[R, Unit]): RComp[R, Unit] =
    message.flatMap(emitPathAnd(path, _)(ifEmitted))

  def writeMetadata(): RComp[R, Unit] =
    emitPath(ModulePaths.metadata, module.metadata).unit

  def writeReferences(): RComp[R, Unit] =
    emitPath(ModulePaths.referencedModules, module.references).unit

  def writeNamespaces(): RComp[R, Unit] =
    module.namespaces.foreach { ns =>
      writeNamespace(ns.id)
    }

  def writeNamespace(id: Int): RComp[R, Unit] =
    module.namespaceElements(id).foreach {
      case GlobalDeclarationElement.TraitElement(traitElement) => writeTrait(traitElement.id)
      case GlobalDeclarationElement.ClassElement(classElement) => writeClass(classElement.id)
      case GlobalDeclarationElement.DataConstructorElement(dataConstructorElement) => writeDataCtor(dataConstructorElement.id)
      case GlobalDeclarationElement.FunctionElement(functionElement) => writeFunc(functionElement.id)
    }

  def writeTrait(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getTraitDef(id).flatMap { traitDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.traitTypeName, id), traitDef)(
          for {
            _ <- writeParameters(traitDef.signature.parameters)
            _ <- writeTraitTypes(traitDef.signature.baseTraits)
            _ <- writeMethods(traitDef.methods)
            _ <- writeMethods(traitDef.staticMethods)
          } yield ()
        )
      }
    else
      module.getTraitRef(-id).flatMap { traitRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.traitTypeName, id), traitRef)(
          writeErasedSigParamOnly(traitRef.signature)
        )
      }

  def writeClass(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getClassDef(id).flatMap { classDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.classTypeName, id), classDef)(
          for {
            _ <- writeParameters(classDef.signature.parameters)
            _ <- writeTraitTypes(classDef.signature.baseTraits)
            _ <- ZIO.foreach(classDef.signature.baseClass)(writeClassType)
            _ <- ZIO.foreach_(classDef.fields) { field =>
              writeExpression(field.fieldType)
            }
            _ <- writeMethods(classDef.methods)
            _ <- writeMethods(classDef.staticMethods)
            _ <- writeClassConstructors(classDef.constructors)
          } yield ()
        )
      }
    else
      module.getClassRef(-id).flatMap { classRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.classTypeName, id), classRef)(
          writeErasedSigParamOnly(classRef.signature)
        )
      }

  def writeDataCtor(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getDataConstructorDef(id).flatMap { dataCtorDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.dataCtorTypeName, id), dataCtorDef)(
          for {
            _ <- writeParameters(dataCtorDef.signature.parameters)
            _ <- writeTraitTypes(Vector(dataCtorDef.signature.instanceType))
            _ <- writeMethods(dataCtorDef.methods)
          } yield ()
        )
      }
    else
      module.getDataConstructorRef(-id).flatMap { dataCtorRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.dataCtorTypeName, id), dataCtorRef)(
          writeErasedSigParamOnly(dataCtorRef.signature)
        )
      }

  def writeFunc(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getFunctionDef(id).flatMap { funcDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.funcTypeName, id), funcDef)(
          for {
            _ <- writeParameters(funcDef.signature.parameters)
            _ <- writeExpression(funcDef.signature.returnType)
            _ <- writeFunctionBody(id, ModulePaths.funcTypeName, funcDef.body, module.getFunctionPayload)
          } yield ()
        )
      }
    else
      module.getFunctionRef(-id).flatMap { funcRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.funcTypeName, id), funcRef)(
          writeErasedSig(funcRef.signature)
        )
      }

  def writeMethod(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getMethodDef(id).flatMap { methodDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.methodTypeName, id), methodDef)(
          for {
            _ <- writeParameters(methodDef.signature.parameters)
            _ <- writeExpression(methodDef.signature.returnType)
            _ <- writeFunctionBody(id, ModulePaths.methodTypeName, methodDef.body, module.getMethodPayload)
          } yield ()
        )
      }
    else
      module.getMethodRef(-id).flatMap { methodRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.methodTypeName, id), methodRef)(
          writeErasedSig(methodRef.signature)
        )
      }

  def writeClassConstructor(id: Int): RComp[R, Unit] =
    if(id >= 0)
      module.getClassConstructorDef(id).flatMap { ctorDef =>
        emitPathAnd(ModulePaths.elementDef(ModulePaths.classCtorTypeName, id), ctorDef)(
          for {
            _ <- writeParameters(ctorDef.signature.parameters)
            _ <- ZIO.foreach(ctorDef.body) {
              case ClassConstructorBody(ClassConstructorBody.BodyType.ExpressionBody(expressionBody)) =>
                for {
                  _ <- ZIO.foreach_(expressionBody.preInitStatements) { stmt =>
                    writeExpression(stmt.expr)
                  }
                  _ <- ZIO.foreach(expressionBody.baseConstructorCall) { baseCall =>
                    writeClassConstructor(baseCall.baseConstructorId) *>
                    writeExpressions(baseCall.args) *>
                    writeClassType(baseCall.instanceClassType)
                  }
                } yield ()

              case ClassConstructorBody(ClassConstructorBody.BodyType.ExternalImplementation(_)) =>
                ZIO.foreach_(module.metadata.platforms) { platformId =>
                  emitPath(ModulePaths.extern(platformId, ModulePaths.classCtorTypeName, id), module.getClassConstructorPayload(id, platformId))
                }

              case ClassConstructorBody(ClassConstructorBody.BodyType.Empty) => IO.unit
            }
          } yield ()
        )
      }
    else
      module.getMethodRef(-id).flatMap { methodRef =>
        emitPathAnd(ModulePaths.elementRef(ModulePaths.methodTypeName, id), methodRef)(
          writeErasedSig(methodRef.signature)
        )
      }


  def writeParameters(params: Vector[Parameter]): RComp[R, Unit] =
    ZIO.foreach_(params) { param =>
      ZIO.foreach_(param.elements) { elem =>
        writeExpression(elem.paramType)
      }
    }


  def writeTraitTypes(traitTypes: Vector[TraitType]): RComp[R, Unit] =
    ZIO.foreach_(traitTypes) { traitType =>
      ZIO.foreach_(traitType.typeArguments)(writeExpression)
    }

  def writeClassType(classType: ClassType): RComp[R, Unit] =
    writeClass(classType.classId) *>
      writeExpressions(classType.typeArguments)

  def writeFunctionBody(id: Int, typeName: String, bodyOpt: Option[FunctionBody], getPayload: (Int, String) => RCompStream[R, Byte]): RComp[R, Unit] =
    ZIO.foreach(bodyOpt) {
      case FunctionBody(FunctionBody.BodyType.ExpressionBody(expr)) =>
        writeExpression(expr)

      case FunctionBody(FunctionBody.BodyType.ExternalImplementation(_)) =>
        ZIO.foreach_(module.metadata.platforms) { platformId =>
          emitPath(ModulePaths.extern(platformId, typeName, id), getPayload(id, platformId))
        }

      case FunctionBody(FunctionBody.BodyType.Empty) => IO.unit
    }.unit

  def writeMethods(methods: Vector[MethodMember]): RComp[R, Unit] =
    ZIO.foreach_(methods) { method =>
      writeMethod(method.id)
    }

  def writeClassConstructors(ctors: Vector[MethodMember]): RComp[R, Unit] =
    ZIO.foreach_(ctors) { ctor =>
      writeClassConstructor(ctor.id)
    }

  def writeExpressions(expressions: Vector[Expression]): RComp[R, Unit] =
    ZIO.foreach_(expressions)(writeExpression)

  def writeErasedSigParamOnly(sig: ErasedSignatureParameterOnly): RComp[R, Unit] =
    ZIO.foreach_(sig.parameterTypes)(writeSigType)

  def writeErasedSig(sig: ErasedSignature): RComp[R, Unit] =
    ZIO.foreach_(sig.parameterTypes)(writeSigType) *> writeSigType(sig.resultType)

  def writeSigType(t: SigType): RComp[R, Unit] =
    t.sigType match {
      case SigType.SigType.Empty | SigType.SigType._Empty(_) => IO.unit
      case SigType.SigType.ClassType(SigTypeClass(classId, args)) =>
        writeClass(classId) *> ZIO.foreach_(args)(writeSigType)

      case SigType.SigType.TraitType(SigTypeTrait(traitId, args)) =>
        writeTrait(traitId) *> ZIO.foreach_(args)(writeSigType)

      case SigType.SigType.DataCtorType(SigTypeDataConstructor(ctorId, args)) =>
        writeDataCtor(ctorId) *> ZIO.foreach_(args)(writeSigType)

      case SigType.SigType.TupleType(SigTypeTuple(elements)) =>
        ZIO.foreach_(elements)(writeSigType)

      case SigType.SigType.FunctionType(SigTypeFunction(argumentType, resultType)) =>
        writeSigType(argumentType) *> writeSigType(resultType)
    }

  def writeExpression(expression: Expression): RComp[R, Unit] =
    ZIO.foreach_(expression.args)(writeExpression) *> (expression.exprType match {
      case Expression.ExprType.TraitType(traitId) => writeTrait(traitId)
      case Expression.ExprType.ClassType(classId) => writeClass(classId)
      case Expression.ExprType.DataConstructorType(ctorId) => writeDataCtor(ctorId)
      case Expression.ExprType.TypeOfType(_) => IO.unit
      case Expression.ExprType.TypeN(TypeN(_, subtypeConstraint, supertypeConstraint)) =>
        ZIO.foreach(subtypeConstraint)(writeExpression) *> ZIO.foreach(supertypeConstraint)(writeExpression).unit

      case Expression.ExprType.FunctionType(_) => IO.unit
      case Expression.ExprType.UnionType(_) => IO.unit
      case Expression.ExprType.IntersectionType(_) => IO.unit

      case Expression.ExprType.ExistentialType(variable) =>
        writeExpression(variable.varType)

      case Expression.ExprType.ClassConstructorCall(ctorId) => writeClassConstructor(ctorId)
      case Expression.ExprType.DataConstructorCall(_) => IO.unit
      case Expression.ExprType.EnsureExecuted(_) => IO.unit
      case Expression.ExprType.FunctionCall(funcId) => writeFunc(funcId)
      case Expression.ExprType.FunctionObjectCall(_) => IO.unit
      case Expression.ExprType.IfElse(_) => IO.unit
      case Expression.ExprType.LetBinding(variable) =>
        writeExpression(variable.varType)

      case Expression.ExprType.LoadConstantBool(_) => IO.unit
      case Expression.ExprType.LoadConstantInt(_) => IO.unit
      case Expression.ExprType.LoadConstantString(_) => IO.unit

      case Expression.ExprType.LoadLambda(variable) =>
        writeExpression(variable.varType)

      case Expression.ExprType.LoadTuple(_) => IO.unit
      case Expression.ExprType.LoadTupleElement(_) => IO.unit

      case Expression.ExprType.LoadUnit(_) => IO.unit
      case Expression.ExprType.LoadVariable(_) => IO.unit // not needed because the declaration would have already been seen

      case Expression.ExprType.MethodCall(methodId) =>
        writeMethod(methodId)

      case Expression.ExprType.PatternMatch(patternMatch) =>
        ZIO.foreach_(patternMatch.patterns)(writePatternExpr)

      case Expression.ExprType.Sequence(_) => IO.unit
      case Expression.ExprType.StoreVariable(_) => IO.unit

    })

  def writePatternExpr(patternExpr: PatternExpr): RComp[R, Unit] =
    patternExpr.pattern match {
      case PatternExpr.Pattern.Empty => IO.unit
      case PatternExpr.Pattern.DataDeconstructor(DataDeconstructPattern(dataConstructorId, args)) =>
        writeDataCtor(dataConstructorId) *> ZIO.foreach_(args)(writePatternExpr)

      case PatternExpr.Pattern.Binding(variable) =>
        writeExpression(variable.varType)

      case PatternExpr.Pattern.CastBinding(variable) =>
        writeExpression(variable.varType)
    }
}

object ZipModuleWriter {
  def writeModule(module2: ArgonModuleSerialized): RCompStream[MaybeBlocking, Byte] = {
    ZipCreator.zipEntries(
      ZStream.unwrap(
        for {
          emittedPaths <- Ref.make(Set.empty[String])
        } yield (new Source[MaybeBlocking, CompilationError, ZipEntryInfo[MaybeBlocking, Throwable]] {
          override def foreach(f: ZipEntryInfo[MaybeBlocking, Throwable] => ZIO[MaybeBlocking, CompilationError, Unit]): ZIO[MaybeBlocking, CompilationError, Unit] = {
            val writer: ZipModuleWriter[MaybeBlocking] = new ZipModuleWriter[MaybeBlocking] {
              override val module: ArgonModuleSerialized = module2

              override def emitPath(path: String, data: RCompStream[MaybeBlocking, Byte]): RComp[MaybeBlocking, Boolean] =
                emittedPaths.modify { paths =>
                  if(paths.contains(path)) (false, paths)
                  else (true, paths + path)
                }.flatMap { shouldEmit =>
                  if(shouldEmit) f(ZipEntryInfo(path, data)).as(true)
                  else IO.succeed(false)
                }
            }

            for {
              _ <- writer.writeMetadata()
              _ <- writer.writeReferences()
              _ <- writer.writeNamespaces()
            } yield ()
          }

        }).toZStream
      )
    ).catchAll(Compilation.unwrapThrowableStream)
  }
}
