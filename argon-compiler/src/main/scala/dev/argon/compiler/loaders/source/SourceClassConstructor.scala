package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util._
import cats._
import cats.implicits._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.StandardTypeLoaders

object SourceClassConstructor {

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (ownerClass2: ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier])
  (stmt: parser.ClassConstructorDeclarationStmt)
  (desc: ClassConstructorDescriptor)
  : context2.Comp[ClassConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier]] = {
    import context2._

    for {
      sigCache <- Compilation[Comp].createCache[context2.signatureContext.Signature[ClassConstructor.ResultInfo]]

    } yield new ClassConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      import context.typeSystem
      import context.scopeContext.ScopeExtensions
      import context.scopeContext.Scope
      import typeSystem.{ArExpr, TType}

      override val descriptor: ClassConstructorDescriptor = desc
      override val fileId: FileID = env.fileSpec.fileID

      override val effectInfo: EffectInfo = EffectInfo.pure

      override val ownerClass: ArClass[context.type, DeclarationPayloadSpecifier] = ownerClass2

      override lazy val signature: Comp[context.signatureContext.Signature[ClassConstructor.ResultInfo]] =
        sigCache(
          SourceSignatureCreator.fromParameters[ClassConstructor.ResultInfo](context2)(
            env(context)(effectInfo, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator)
        )

      override lazy val payload: Comp[context.TClassConstructorImplementation] =
        for {
          sig <- signature
          env2 = env(context)(effectInfo, descriptor)
          env3 = env2.copy(scope = env2.scope.addVariables(
            sig.unsubstitutedParameters.flatMap(_.tupleVars)
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
      (converted: Vector[typeSystem.ClassConstructorStatement])
      (initializedFields: Set[FieldDescriptor])
      (body: WithSource[Vector[WithSource[parser.Stmt]]])
      : Comp[typeSystem.ClassConstructorBody] = {

        def convertUnconverted: Comp[(Vector[typeSystem.ClassConstructorStatement], ExpressionConverter.Env[context.type, Scope])] =
          if(unconverted.value.isEmpty)
            (converted, env).pure[Comp]
          else
            ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { newStmt =>

              (converted :+ typeSystem.ClassConstructorStatementExpr(newStmt), ExpressionScopeExtractor.addDeclarationsFrom(context)(newStmt, env))
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
                  Compilation[Comp].forErrors(CompilationError.FieldReinitializedError(CompilationMessageSource.SourceFile(env2.fileSpec, location)))
                else
                  ExpressionConverter.convertExpression(context)(env2)(field.varType)(value).flatMap { valueExpr =>
                    val initStmt = typeSystem.InitializeFieldStatement(field, valueExpr)
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
                Compilation[Comp].forErrors(CompilationError.FieldNotInitializedError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
              else
                convertUnconverted.flatMap { case (newConverted, env2) =>
                  ownerClass.signature.flatMap { ownerSig =>
                    ((baseCtorExprOpt, ownerSig.unsubstitutedResult.baseTypes.baseClass) match {
                      case (None, Some(_)) =>
                        Compilation[Comp].forErrors(
                          CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, location))
                        )

                      case (Some(_), None) =>
                        Compilation[Comp].forErrors(
                          CompilationError.InvalidBaseConstructorCall(CompilationMessageSource.SourceFile(env.fileSpec, location))
                        )

                      case (None, None) =>
                        None.pure[Comp]

                      case (Some(baseCtorExpr), Some(baseClass)) =>
                        ExpressionConverter.convertExpression(context)(env2.copy(allowAbstractConstructor = true))(typeSystem.fromSimpleType(baseClass))(baseCtorExpr).flatMap {
                          case baseCall: typeSystem.ClassConstructorCall => Some(baseCall).pure[Comp]
                          case _ =>
                            Compilation[Comp].forErrors(
                              CompilationError.InvalidBaseConstructorCall(CompilationMessageSource.SourceFile(env.fileSpec, location))
                            )
                        }
                    }).flatMap { baseCall =>
                      ExpressionConverter.convertStatementList(context)(env2)(unitType)(nextBody(tail)).map { endExpr =>
                        typeSystem.ClassConstructorBody(newConverted, baseCall, endExpr)
                      }
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
              if(ownerSig.unsubstitutedResult.baseTypes.baseClass.isDefined)
                Compilation[Comp].forErrors(
                  CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, body.location))
                )
              else
                ownerClass.fields.flatMap { fields =>
                  if(fields.size =!= initializedFields.size)
                    Compilation[Comp].forErrors(CompilationError.FieldNotInitializedError(CompilationMessageSource.SourceFile(env.fileSpec, unconverted.location)))
                  else
                    ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { endExpr =>
                      typeSystem.ClassConstructorBody(converted, None, endExpr)
                    }
                }
            }

        }
      }

      private def findField(name: String, location: SourceLocation): Comp[typeSystem.FieldVariable] =
        ownerClass.fields.flatMap { fields =>
          Compilation[Comp].requireSome(fields.find { _.name.name === name })(
            CompilationError.FieldNotFound(name, CompilationMessageSource.SourceFile(env.fileSpec, location))
          )
        }

    }
  }

  val resultCreator: ResultCreator[ClassConstructor.ResultInfo] =  new ResultCreator[ClassConstructor.ResultInfo] {
    override def createResult
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : context.Comp[ClassConstructor.ResultInfo[context.type, context.typeSystem.type]] = {
      import context._
      ClassConstructor.ResultInfo[context.type, context.typeSystem.type]().pure[Comp]
    }
  }
}
