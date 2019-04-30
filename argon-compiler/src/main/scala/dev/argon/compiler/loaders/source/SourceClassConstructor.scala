package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util._
import scalaz._
import Scalaz._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.StandardTypeLoaders

object SourceClassConstructor {

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (ownerClass2: ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier])
  (stmt: parser.ClassConstructorDeclarationStmt)
  (desc: ClassConstructorDescriptor)
  : TComp[ClassConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier]] = for {
    sigCache <- Compilation[TComp].createCache[context2.signatureContext.Signature[ClassConstructor.ResultInfo]]

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

    override lazy val signature: TComp[context.signatureContext.Signature[ClassConstructor.ResultInfo]] =
      sigCache(
        SourceSignatureCreator.fromParameters[TComp, ClassConstructor.ResultInfo](context2)(
          env(context)(effectInfo, descriptor)
        )(descriptor)(stmt.parameters)(resultCreator)
      )

    override lazy val payload: TComp[context.TClassConstructorImplementation] =
      for {
        sig <- signature
        env2 = env(context)(effectInfo, descriptor)
        env3 = env2.copy(scope = env2.scope.addVariables(
          sig.unsubstitutedParameters.flatMap(_.tupleVars)
        ))

        unitType <- StandardTypeLoaders.loadUnitType(context)(
          CompilationMessageSource.SourceFile(env.fileSpec, stmt.body.location)
        )(env3.currentModule)(env3.referencedModules)

        body <- convertCtorBody(unitType)(env3)(WithSource(Vector(), SourceLocation(stmt.body.location.start, stmt.body.location.start)))(Vector())(stmt.body)
      } yield context.createClassConstructorBodyImplementation(body)

    private def convertCtorBody
    (unitType: TType)
    (env: ExpressionConverter.Env[context.type, Scope])
    (unconverted: WithSource[Vector[WithSource[parser.Stmt]]])
    (converted: Vector[typeSystem.ClassConstructorStatement])
    (body: WithSource[Vector[WithSource[parser.Stmt]]])
    : TComp[typeSystem.ClassConstructorBody] = {

      def convertUnconverted: TComp[(Vector[typeSystem.ClassConstructorStatement], ExpressionConverter.Env[context.type, Scope])] =
        if(unconverted.value.isEmpty)
          (converted, env).point[TComp]
        else
          ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { newStmt =>
            
            def addNewVariables(stmt: ArExpr, env: ExpressionConverter.Env[context.type, Scope]): ExpressionConverter.Env[context.type, Scope] =
              stmt match {
                case typeSystem.LetBinding(variable, _, next) =>
                  addNewVariables(next, env.copy(scope = env.scope.addVariable(variable)))

                case typeSystem.Sequence(_, next) =>
                  addNewVariables(next, env)

                case _ => env
              }

            (converted :+ typeSystem.ClassConstructorStatementExpr(newStmt), addNewVariables(newStmt, env))
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
              ExpressionConverter.convertExpression(context)(env2)(field.varType)(value).flatMap { valueExpr =>
                val initStmt = typeSystem.InitializeFieldStatement(field, valueExpr)
                val env3 = env2.copy(scope = env2.scope.addVariable(field))

                val newUnconvertedLoc = tail.headOption.map { _.location.start }.getOrElse { body.location.end }
                val newUnconverted = WithSource(Vector(), SourceLocation(newUnconvertedLoc, newUnconvertedLoc))
                convertCtorBody(unitType)(env3)(newUnconverted)(newConvertedNoInit :+ initStmt)(nextBody(tail))
              }
            }
          }

        case WithSource(parser.InitializeStmt(None, None), _) +: tail =>
          val newUnconverted = WithSource(
            unconverted.value ++ tail,
            SourceLocation.merge(unconverted.location, body.location)
          )
          convertCtorBody(unitType)(env)(newUnconverted)(converted)(nextBody(Vector()))

        case WithSource(parser.InitializeStmt(thisName, baseCtorExprOpt), location) +: tail =>
          convertUnconverted.flatMap { case (newConverted, env2) =>
            ownerClass.signature.flatMap { ownerSig =>
              ((baseCtorExprOpt, ownerSig.unsubstitutedResult.baseTypes.baseClass) match {
                case (None, Some(_)) =>
                  Compilation[TComp].forErrors(
                    CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, location))
                  )

                case (Some(_), None) =>
                  Compilation[TComp].forErrors(
                    CompilationError.InvalidBaseConstructorCall(CompilationMessageSource.SourceFile(env.fileSpec, location))
                  )

                case (None, None) =>
                  None.point[TComp]

                case (Some(baseCtorExpr), Some(baseClass)) =>
                  ExpressionConverter.convertExpression(context)(env2.copy(allowAbstractConstructor = true))(typeSystem.fromSimpleType(baseClass))(baseCtorExpr).flatMap {
                    case baseCall: typeSystem.ClassConstructorCall => Some(baseCall).point[TComp]
                    case _ =>
                      Compilation[TComp].forErrors(
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

        case stmt +: tail =>
          val newUnconverted = WithSource(
            unconverted.value :+ stmt,
            SourceLocation.merge(unconverted.location, stmt.location)
          )
          convertCtorBody(unitType)(env)(newUnconverted)(converted)(nextBody(tail))

        case Vector() =>
          ownerClass.signature.flatMap { ownerSig =>
            if(ownerSig.unsubstitutedResult.baseTypes.baseClass.isDefined)
              Compilation[TComp].forErrors(
                CompilationError.BaseConstructorNotCalled(CompilationMessageSource.SourceFile(env.fileSpec, body.location))
              )
            else
              ExpressionConverter.convertStatementList(context)(env)(unitType)(unconverted).map { endExpr =>
                typeSystem.ClassConstructorBody(converted, None, endExpr)
              }
          }

      }
    }

    private def findField(name: String, location: SourceLocation): TComp[typeSystem.FieldVariable] =
      ownerClass.fields.flatMap { fields =>
        Compilation[TComp].requireSome(fields.find { _.name.name === name })(
          CompilationError.FieldNotFound(name, CompilationMessageSource.SourceFile(env.fileSpec, location))
        )
      }

  }

  val resultCreator: ResultCreator[ClassConstructor.ResultInfo] =  new ResultCreator[ClassConstructor.ResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[ClassConstructor.ResultInfo[context.type, context.typeSystem.type]] =
      ClassConstructor.ResultInfo[context.type, context.typeSystem.type]().point[TComp]
  }
}
