package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util._
import cats.{Id => _, _}
import cats.implicits._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.expr.ArExpr.ClassConstructorCall
import dev.argon.compiler.expr._
import dev.argon.compiler.loaders.StandardTypeLoaders
import shapeless.{Id, Nat}
import zio.IO

object SourceClassConstructor {

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (ownerClass2: ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier])
  (stmt: parser.ClassConstructorDeclarationStmt)
  (desc: ClassConstructorDescriptor)
  : Comp[ClassConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier]] =
    for {
      sigCache <- ValueCache.make[ErrorList, context2.signatureContext.Signature[ClassConstructor.ResultInfo, _ <: Nat]]

    } yield new ClassConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      import context.typeSystem
      import context.scopeContext.ScopeExtensions
      import context.scopeContext.Scope
      import typeSystem.{SimpleExpr, TType}

      override val descriptor: ClassConstructorDescriptor = desc
      override val fileId: FileID = env.fileSpec.fileID

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

      override val ownerClass: ArClass[context.type, DeclarationPayloadSpecifier] = ownerClass2

      override lazy val signatureUnsubstituted: Comp[context.signatureContext.Signature[ClassConstructor.ResultInfo, _ <: Nat]] =
        sigCache.get(
          SourceSignatureCreator.fromParameters[ClassConstructor.ResultInfo](context2)(
            env(context)(effectInfo, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator)
        )

      override lazy val payload: Comp[context.TClassConstructorImplementation] =
        for {
          sig <- signatureUnsubstituted
          env2 = env(context)(effectInfo, descriptor)
          env3 = env2.copy(scope = env2.scope.addParameters(
            sig.unsubstitutedParameters
          ))

          unitType <- StandardTypeLoaders.loadUnitType(context)(
            CompilationMessageSource.SourceFile(env.fileSpec, stmt.body.location)
          )(env3.currentModule)(env3.referencedModules)

          body <- convertCtorBody(unitType)(env3)(WithSource(Vector(), SourceLocation(stmt.body.location.start, stmt.body.location.start)))(Vector())(Set())(stmt.body)
        } yield context.createClassConstructorBodyImplementation(body)

      private def convertCtorBody
      (unitType: TType)
      (env: ExpressionConverter.Env[context.type, Scope])
      (unconverted: WithSource[Vector[WithSource[parser.Stmt]]])
      (converted: Vector[ClassConstructorStatement[context.type]])
      (initializedFields: Set[FieldDescriptor])
      (body: WithSource[Vector[WithSource[parser.Stmt]]])
      : Comp[ClassConstructorBody[context.type]] = {

        def convertUnconverted: Comp[(Vector[ClassConstructorStatement[context.type]], ExpressionConverter.Env[context.type, Scope])] =
          if(unconverted.value.isEmpty)
            IO.succeed((converted, env))
          else
            ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { newStmt =>

              (converted :+ ClassConstructorStatementExpr(newStmt), ExpressionScopeExtractor.addDeclarationsFrom(context)(newStmt, env))
            }

        def nextBody(tail: Vector[WithSource[parser.Stmt]]) = {
          val nextBodyLoc = SourceLocation(
            tail.headOption.map { _.location.start }.getOrElse { body.location.end },
            body.location.end
          )
          WithSource(tail, nextBodyLoc)
        }

        body.value match {
          case WithSource(parser.FieldInitializationStmt(name, value), location) +: tail =>
            convertUnconverted.flatMap { case (newConvertedNoInit, env2) =>
              findField(name, location).flatMap { field =>
                if(initializedFields.contains(field.descriptor))
                  Compilation.forErrors(CompilationError.FieldReinitializedError(CompilationMessageSource.SourceFile(env2.fileSpec, location)))
                else if(Mutability.toIsMutable(field.mutability) && effectInfo.isPure)
                  Compilation.forErrors(CompilationError.MutableVariableNotPureError(field.name, CompilationMessageSource.SourceFile(env2.fileSpec, location)))
                else
                  ExpressionConverter.convertExpression(context)(env2)(field.varType)(value).flatMap { valueExpr =>
                    val initStmt = InitializeFieldStatement(field, valueExpr)
                    val env3 = env2.copy(scope = env2.scope.addVariable(field))

                    val newUnconvertedLoc = tail.headOption.map { _.location.start }.getOrElse { body.location.end }
                    val newUnconverted = WithSource(Vector(), SourceLocation(newUnconvertedLoc, newUnconvertedLoc))
                    convertCtorBody(unitType)(env3)(newUnconverted)(newConvertedNoInit :+ initStmt)(initializedFields + field.descriptor)(nextBody(tail))
                  }
              }
            }

          case WithSource(parser.InitializeStmt(thisName, baseCtorExprOpt), location) +: tail =>
            ownerClass.fields.flatMap { fields =>
              if(fields.size =!= initializedFields.size)
                Compilation.forErrors(CompilationError.FieldNotInitializedError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
              else
                convertUnconverted.flatMap { case (newConverted, env2) =>
                  ownerClass.signature
                    .flatMap { ownerSig =>
                      ownerSig.unsubstitutedResult.baseTypes
                    }
                    .flatMap { baseTypes =>
                      ((baseCtorExprOpt, baseTypes.baseClass) match {
                        case (None, Some(_)) =>
                          Compilation.forErrors(
                            CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, location))
                          )

                        case (Some(_), None) =>
                          Compilation.forErrors(
                            CompilationError.InvalidBaseConstructorCall(CompilationMessageSource.SourceFile(env.fileSpec, location))
                          )

                        case (None, None) =>
                          IO.succeed(None)

                        case (Some(baseCtorExpr), Some(baseClass)) =>
                          ExpressionConverter.convertExpression(context)(env2.copy(allowAbstractConstructor = true))(typeSystem.fromSimpleType(baseClass))(baseCtorExpr).flatMap {
                            case baseCall: ClassConstructorCall[context.type, Id] => IO.succeed(Some(baseCall))
                            case _ =>
                              Compilation.forErrors(
                                CompilationError.InvalidBaseConstructorCall(CompilationMessageSource.SourceFile(env.fileSpec, location))
                              )
                          }
                      })
                    }
                    .flatMap { baseCall =>
                      ExpressionConverter.convertStatementList(context)(env2)(unitType)(nextBody(tail)).map { endExpr =>
                        ClassConstructorBody(newConverted, baseCall, endExpr)
                      }
                    }

                }
            }

          case stmt +: tail =>
            val newUnconverted = WithSource(
              unconverted.value :+ stmt,
              SourceLocation.merge(unconverted.location, stmt.location)
            )
            convertCtorBody(unitType)(env)(newUnconverted)(converted)(initializedFields)(nextBody(tail))

          case Vector() =>
            ownerClass.signature.flatMap { ownerSig =>
              ownerSig.unsubstitutedResult.baseTypes.flatMap { baseTypes =>
                if(baseTypes.baseClass.isDefined)
                  Compilation.forErrors(
                    CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, body.location))
                  )
                else
                  ownerClass.fields.flatMap { fields =>
                    if(fields.size =!= initializedFields.size)
                      Compilation.forErrors(CompilationError.FieldNotInitializedError(CompilationMessageSource.SourceFile(env.fileSpec, unconverted.location)))
                    else
                      ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { endExpr =>
                        ClassConstructorBody(converted, None, endExpr)
                      }
                  }
              }
            }

        }
      }

      private def findField(name: String, location: SourceLocation): Comp[FieldVariable[context.type, Id]] =
        ownerClass.fields.flatMap { fields =>
          Compilation.requireSome(fields.find { _.name.name === name })(
            CompilationError.FieldNotFound(name, CompilationMessageSource.SourceFile(env.fileSpec, location))
          )
        }


      private def resultCreator: ResultCreator.Aux[context2.type, ClassConstructor.ResultInfo] =  new ResultCreator[ClassConstructor.ResultInfo] {

        override val context: context2.type = context2

        override def createResult
        (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
        : Comp[ClassConstructor.ResultInfo[context.type, context.typeSystem.TTypeWrapper]] =
          IO.succeed(ClassConstructor.ResultInfo[context.type, context.typeSystem.TTypeWrapper]())
      }

    }

}
