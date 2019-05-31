package dev.argon.backend.js

import java.io.PrintWriter

import org.apache.commons.text.StringEscapeUtils
import cats._
import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.util.builder.Builder

final case class JSModule(statements: Vector[JSModuleStatement])

sealed trait JSModuleStatement

sealed trait JSExportStatement extends JSModuleStatement
final case class JSExportDefaultStatement(expr: JSExpression) extends JSExportStatement
final case class JSExportDeclaration(declaration: JSDeclarationStatement) extends JSExportStatement

sealed trait JSImportStatement extends JSModuleStatement
final case class JSImportDefaultStatement(defaultExport: JSIdentifier, moduleName: String) extends JSImportStatement
final case class JSImportAllStatement(defaultExport: Option[JSIdentifier], name: JSIdentifier, moduleName: String) extends JSImportStatement

final case class JSModuleRaw(code: String) extends JSModuleStatement

sealed trait JSStatement extends JSModuleStatement

sealed trait JSDeclarationStatement extends JSStatement
final case class JSConst(declarations: NonEmptyList[JSDeclaration]) extends JSDeclarationStatement
final case class JSLet(declarations: NonEmptyList[JSDeclaration]) extends JSDeclarationStatement
final case class JSFunctionStatement(name: JSIdentifier, parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSDeclarationStatement

sealed trait JSBinding
sealed trait JSBindingNonEmpty extends JSBinding
final case class JSBindingIdentifier(identifier: JSIdentifier) extends JSBindingNonEmpty
final case class JSArrayDestructBinding(bindings: Vector[JSBinding]) extends JSBindingNonEmpty

sealed trait JSDeclaration
final case class JSDeclareNewVariable(binding: JSBindingNonEmpty) extends JSDeclaration
final case class JSDeclareInit(binding: JSBindingNonEmpty, value: JSExpression) extends JSDeclaration

sealed trait JSFunctionParameterList
case object JSFunctionEmptyParameterList extends JSFunctionParameterList
final case class JSFunctionParameter(name: JSBindingNonEmpty, next: JSFunctionParameterList) extends JSFunctionParameterList
final case class JSFunctionRestParameters(name: JSBindingNonEmpty) extends JSFunctionParameterList

final case class JSIfElseStatement(condition: JSExpression, ifBody: Vector[JSStatement], elseBody: Vector[JSStatement]) extends JSStatement
final case class JSReturn(value: JSExpression) extends JSStatement

sealed trait JSExpression extends JSStatement

final case class JSExpressionRaw(code: String) extends JSExpression

final case class JSObjectLiteral(members: Vector[JSObjectMember]) extends JSExpression

sealed trait JSObjectMember
final case class JSObjectProperty(name: String, value: JSExpression) extends JSObjectMember
final case class JSObjectGetProperty(name: String, body: Vector[JSStatement]) extends JSObjectMember
final case class JSObjectComputedProperty(name: JSExpression, value: JSExpression) extends JSObjectMember

final case class JSIdentifier(id: String) extends JSExpression

final case class JSAssignment(left: JSExpression, right: JSExpression) extends JSExpression
final case class JSPropertyAccessDot(expr: JSExpression, prop: JSIdentifier) extends JSExpression
final case class JSPropertyAccessBracket(expr: JSExpression, prop: JSExpression) extends JSExpression
final case class JSString(value: String) extends JSExpression
final case class JSBigInt(value: BigInt) extends JSExpression
final case class JSFunctionCall(function: JSExpression, args: Vector[JSExpression]) extends JSExpression
final case class JSNewCall(function: JSExpression, args: Vector[JSExpression]) extends JSExpression
final case class JSFunctionExpression(name: Option[JSIdentifier], parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSExpression
final case class JSArrowFunctionExpr(parameters: JSFunctionParameterList, body: JSExpression) extends JSExpression
final case class JSArrowFunctionStmts(parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSExpression
final case class JSArrayLiteral(values: Vector[JSExpression]) extends JSExpression

case object JSNull extends JSExpression
case object JSThis extends JSExpression

object JSAst {

  def writeModule[F[_]](module: JSModule)(implicit builder: Builder[F, String]): F[Unit] =
    new WriteImpl[F].writeModule(module)

  private class WriteImpl[F[_]](implicit builder: Builder[F, String]) {

    def write(s: String): F[Unit] = builder.append(s)
    
    def writeModule(module: JSModule): F[Unit] =
      module.statements.traverse_(writeModuleStatement)

    def writeModuleStatement(stmt: JSModuleStatement): F[Unit] =
      stmt match {
        case JSExportDeclaration(declaration) =>
          for {
            _ <- write("export ")
            _ <- writeStatement(declaration) 
          } yield ()

        case JSExportDefaultStatement(expr) =>
          for {
            _ <- write("export default")
            _ <- writeExprParen(expr)
            _ <- write(";")
          } yield ()

        case JSImportDefaultStatement(defaultExport, moduleName) =>
          for {
            _ <- write("import ")
            _ <- writeIdentifier(defaultExport)
            _ <- write(" from ")
            _ <- writeString(moduleName)
            _ <- write(";")
          } yield ()

        case JSImportAllStatement(defaultExport, name, moduleName) =>
          for {
            _ <- write("import ")
            _ <- defaultExport.traverse_ { defaultId =>
              for {
                _ <- writeIdentifier(defaultId)
                _ <- write(", ")
              } yield ()
            }
            _ <- write("* as ")
            _ <- writeIdentifier(name)
            _ <- write(" from ")
            _ <- writeString(moduleName)
            _ <- write(";")
          } yield ()

        case JSModuleRaw(code) =>
          write(code)

        case stmt: JSStatement =>
          writeStatement(stmt)
      }

    def writeStatement(stmt: JSStatement): F[Unit] =
      stmt match {
        case JSConst(bindings) =>
          for {
            _ <- write("const ")
            _ <- writeDeclaration(bindings.head)
            _ <- bindings.tail.toVector.traverse { binding =>
              for {
                _ <- write(", ")
                _ <- writeDeclaration(binding)
              } yield ()
            }
            _ <- write(";")
          } yield ()

        case JSLet(bindings) =>
          for {
            _ <- write("let ")
            _ <- writeDeclaration(bindings.head)

            _ <- bindings.tail.toVector.traverse_ { binding =>
              for {
                _ <- write(", ")
                _ <- writeDeclaration(binding)
              } yield ()
            }
            _ <- write(";")
          } yield ()

        case JSFunctionStatement(name, parameters, body) =>
          for {
            _ <- write("function ")
            _ <- writeIdentifier(name)
            _ <- write("(")
            _ <- writeParameterList(parameters)
            _ <- write("){")
            _ <- body.traverse_(writeStatement)
            _ <- write("}")
          } yield ()

        case JSIfElseStatement(condition, ifBody, elseBody) =>
          for {
            _ <- write("if(")
            _ <- writeExpr(condition)
            _ <- write(") {")
            _ <- ifBody.traverse_(writeStatement)
            _ <- write("} else {")
            _ <- elseBody.traverse_(writeStatement)
            _ <- write("}")
          } yield ()

        case JSReturn(value) =>
          for {
            _ <- write("return ")
            _ <- writeExprParen(value)
            _ <- write(";")
          } yield ()

        case stmt: JSExpression =>
          for {
            _ <- writeExprParen(stmt)
            _ <- write(";")
          } yield ()
      }

    def writeDeclaration(decl: JSDeclaration): F[Unit] =
      decl match {
        case JSDeclareNewVariable(binding) =>
          writeBinding(binding)

        case JSDeclareInit(binding, value) =>
          for {
            _ <- writeBinding(binding)
            _ <- write(" = ")
            _ <- writeExprParen(value)
          } yield ()
      }

    def writeBinding(binding: JSBinding): F[Unit] =
      binding match {
        case JSBindingIdentifier(id) =>
          writeIdentifier(id)

        case JSArrayDestructBinding(bindings) =>
          for {
            _ <- write("[")
            _ <- bindings.traverse_ { binding =>
              for {
                _ <- writeBinding(binding)
                _ <- write(",")
              } yield ()
            }
            _ <- write("]")
          } yield ()
      }

    def writeExprParen(expr: JSExpression): F[Unit] = for {
      _ <- write("(")
      _ <- writeExpr(expr)
      _ <- write(")")
    } yield ()

    def writeExpr(expr: JSExpression): F[Unit] =
      expr match {
        case JSExpressionRaw(code) =>
          write(code)

        case JSObjectLiteral(members) =>
          for {
            _ <- write("{")
            _ <- members.traverse_ {
              case JSObjectProperty(name, value) =>
                for {
                  _ <- writeString(name)
                  _ <- write(":")
                  _ <- writeExprParen(value)
                  _ <- write(",")
                } yield ()
              case JSObjectGetProperty(name, value) =>
                for {
                  _ <- write("get ")
                  _ <- writeString(name)
                  _ <- write("() {")
                  _ <- value.traverse_(writeStatement)
                  _ <- write("},")
                } yield ()
              case JSObjectComputedProperty(name, value) =>
                for {
                  _ <- write("[")
                  _ <- writeExpr(name)
                  _ <- write("]:")
                  _ <- writeExprParen(value)
                  _ <- write(",")
                } yield ()
            }
            _ <- write("}")
          } yield ()

        case expr: JSIdentifier =>
          writeIdentifier(expr)

        case JSAssignment(left, right) =>
          for {
            _ <- writeExprParen(left)
            _ <- write("=")
            _ <- writeExprParen(right)
          } yield ()

        case JSPropertyAccessDot(expr, prop) =>
          for {
            _ <- writeExprParen(expr)
            _ <- write(".")
            _ <- writeIdentifier(prop)
          } yield ()

        case JSPropertyAccessBracket(expr, prop) =>
          for {
            _ <- writeExprParen(expr)
            _ <- write("[")
            _ <- writeExpr(prop)
            _ <- write("]")
          } yield ()

        case JSString(value) =>
          writeString(value)

        case JSBigInt(i) =>
          for {
            _ <- write(i.toString)
            _ <- write("n")
          } yield ()

        case JSFunctionCall(function, args) =>
          for {
            _ <- writeExprParen(function)
            _ <- write("(")
            _ <- args.traverse_ { arg =>
              for {
                _ <- writeExpr(arg)
                _ <- write(",")
              } yield ()
            }
            _ <- write(")")
          } yield ()

        case JSNewCall(function, args) =>
          for {
            _ <- write("new ")
            _ <- writeExprParen(function)
            _ <- write("(")
            _ <- args.traverse_ { arg =>
              for {
                _ <- writeExpr(arg)
                _ <- write(",")
              } yield ()
            }
            _ <- write(")")
          } yield ()

        case JSFunctionExpression(name, parameters, body) =>
          for {
            _ <- write("function ")
            _ <- name.traverse_(writeIdentifier)
            _ <- write("(")
            _ <- writeParameterList(parameters)
            _ <- write("){")
            _ <- body.traverse_(writeStatement)
            _ <- write("}")
          } yield ()

        case JSArrowFunctionExpr(parameters, body) =>
          for {
            _ <- write("(")
            _ <- writeParameterList(parameters)
            _ <- write(") => ")
            _ <- writeExprParen(body)
          } yield ()

        case JSArrowFunctionStmts(parameters, body) =>
          for {
            _ <- write("(")
            _ <- writeParameterList(parameters)
            _ <- write(") => {")
            _ <- body.traverse_(writeStatement)
            _ <- write("}")
          } yield ()


        case JSArrayLiteral(values) =>
          for {
            _ <- write("[")
            _ <- values.traverse_ { value =>
              for {
                _ <- writeExpr(value)
                _ <- write(",")
              } yield ()
            }
            _ <- write("]")
          } yield ()

        case JSNull =>
          write("null")

        case JSThis =>
          write("this")
      }

    def writeIdentifier(identifier: JSIdentifier): F[Unit] =
      write(identifier.id)

    def writeString(str: String): F[Unit] = for {
      _ <- write("\"")
      _ <- write(StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(str))
      _ <- write("\"")
    } yield ()

    def writeParameterList(params: JSFunctionParameterList): F[Unit] =
      params match {
        case JSFunctionEmptyParameterList => ().pure[F]
        case JSFunctionParameter(binding, next) =>
          for {
            _ <- writeBinding(binding)
            _ <- write(",")
            _ <- writeParameterList(next)
          } yield ()

        case JSFunctionRestParameters(binding) =>
          for {
            _ <- write("...")
            _ <- writeBinding(binding)
          } yield ()
      }

  }

}
