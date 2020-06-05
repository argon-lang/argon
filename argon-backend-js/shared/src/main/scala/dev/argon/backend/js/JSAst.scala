package dev.argon.backend.js

import java.io.PrintWriter

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.stream.builder.Source
import dev.argon.util.StringHelpers
import zio.{IO, ZIO}

final case class JSModule(statements: Vector[JSModuleStatement])

sealed trait JSModuleStatement

sealed trait JSExportStatement extends JSModuleStatement
final case class JSExportDefaultStatement(expr: JSExpression) extends JSExportStatement
final case class JSExportDeclaration(declaration: JSDeclarationStatement) extends JSExportStatement

sealed trait JSImportStatement extends JSModuleStatement
final case class JSImportDefaultStatement(defaultExport: JSIdentifier, moduleName: String) extends JSImportStatement
final case class JSImportAllStatement(name: JSIdentifier, moduleName: String) extends JSImportStatement
final case class JSImportDefaultAndAllStatement(defaultExport: JSIdentifier, name: JSIdentifier, moduleName: String) extends JSImportStatement

final case class JSModuleRaw(code: String) extends JSModuleStatement

sealed trait JSStatement extends JSModuleStatement

sealed trait JSDeclarationStatement extends JSStatement
final case class JSConst(declarations: NonEmptyList[JSDeclaration]) extends JSDeclarationStatement
final case class JSLet(declarations: NonEmptyList[JSDeclaration]) extends JSDeclarationStatement
final case class JSFunctionStatement(name: JSIdentifier, parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSDeclarationStatement

sealed trait JSBinding
final case class JSBindingIdentifier(identifier: JSIdentifier) extends JSBinding
final case class JSArrayDestructBinding(bindings: Vector[JSBinding]) extends JSBinding

sealed trait JSDeclaration
final case class JSDeclareNewVariable(binding: JSIdentifier) extends JSDeclaration
final case class JSDeclareInit(binding: JSBinding, value: JSExpression) extends JSDeclaration

sealed trait JSFunctionParameterList
case object JSFunctionEmptyParameterList extends JSFunctionParameterList
final case class JSFunctionParameter(name: JSBinding, next: JSFunctionParameterList) extends JSFunctionParameterList
final case class JSFunctionRestParameters(name: JSBinding) extends JSFunctionParameterList

final case class JSIfElseStatement(condition: JSExpression, ifBody: Vector[JSStatement], elseBody: Vector[JSStatement]) extends JSStatement
final case class JSTryStatement(body: Vector[JSStatement], catchClause: Option[(JSIdentifier, Vector[JSStatement])], finallyBody: Option[Vector[JSStatement]]) extends JSStatement
final case class JSReturn(value: JSExpression) extends JSStatement

final case class JSBlockStatement(body: Vector[JSStatement]) extends JSStatement

sealed trait JSExpression extends JSStatement

final case class JSExpressionRaw(code: String) extends JSExpression

final case class JSObjectLiteral(members: Vector[JSObjectMember]) extends JSExpression

sealed trait JSObjectMember
final case class JSObjectProperty(name: String, value: JSExpression) extends JSObjectMember
final case class JSObjectGetProperty(name: String, body: Vector[JSStatement]) extends JSObjectMember
final case class JSObjectComputedProperty(name: JSExpression, value: JSExpression) extends JSObjectMember

final case class JSIdentifier(id: String) extends JSExpression

final case class JSAssignment(left: JSExpression, right: JSExpression) extends JSExpression
final case class JSPropertyAccessDot(expr: JSExpression, property: JSIdentifier) extends JSExpression
final case class JSPropertyAccessBracket(expr: JSExpression, property: JSExpression) extends JSExpression
final case class JSString(value: String) extends JSExpression
final case class JSBigInt(value: BigInt) extends JSExpression
final case class JSNumberInt(value: Int) extends JSExpression
final case class JSBoolean(value: Boolean) extends JSExpression
final case class JSFunctionCall(function: JSExpression, args: Vector[JSExpression]) extends JSExpression
final case class JSNewCall(function: JSExpression, args: Vector[JSExpression]) extends JSExpression
final case class JSFunctionExpression(name: Option[JSIdentifier], parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSExpression
final case class JSArrowFunctionExpr(parameters: JSFunctionParameterList, body: JSExpression) extends JSExpression
final case class JSArrowFunctionStmts(parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSExpression
final case class JSArrayLiteral(values: Vector[JSExpression]) extends JSExpression

case object JSNull extends JSExpression
case object JSThis extends JSExpression

object JSAst {

  def writeModule[R, E](module: JSModule): Source[R, E, String] = new Source[R, E, String] {

    override def foreach(consume: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      new WriteImpl(consume).writeModule(module)
  }

  private class WriteImpl[R, E](consume: String => ZIO[R, E, Unit]) {

    type F[A] = ZIO[R, E, A]

    def write(s: String): F[Unit] = consume(s)
    
    def writeModule(module: JSModule): F[Unit] =
      ZIO.foreach_(module.statements)(writeModuleStatement)

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

        case JSImportAllStatement(name, moduleName) =>
          for {
            _ <- write("import ")
            _ <- write("* as ")
            _ <- writeIdentifier(name)
            _ <- write(" from ")
            _ <- writeString(moduleName)
            _ <- write(";")
          } yield ()

        case JSImportDefaultAndAllStatement(defaultExport, name, moduleName) =>
          for {
            _ <- write("import ")
            _ <- writeIdentifier(defaultExport)
            _ <- write(", * as ")
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
            _ <- ZIO.foreach_(bindings.tail) { binding =>
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

            _ <- ZIO.foreach_(bindings.tail) { binding =>
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
            _ <- ZIO.foreach_(body)(writeStatement)
            _ <- write("}")
          } yield ()

        case JSIfElseStatement(condition, ifBody, elseBody) =>
          for {
            _ <- write("if(")
            _ <- writeExpr(condition)
            _ <- write(") {")
            _ <- ZIO.foreach_(ifBody)(writeStatement)
            _ <- write("} else {")
            _ <- ZIO.foreach_(elseBody)(writeStatement)
            _ <- write("}")
          } yield ()

        case JSTryStatement(body, catchClause, finallyBody) =>
          for {
            _ <- write("try {")
            _ <- ZIO.foreach_(body)(writeStatement)
            _ <- write("} ")
            _ <- ZIO.foreach(catchClause) {
              case (id, catchBody) =>
                for {
                  _ <- write("catch(")
                  _ <- writeIdentifier(id)
                  _ <- write(") {")
                  _ <- ZIO.foreach_(catchBody)(writeStatement)
                  _ <- write("} ")
                } yield ()
            }
            _ <- ZIO.foreach(finallyBody) { finallyBody =>
                for {
                  _ <- write("finally {")
                  _ <- ZIO.foreach_(finallyBody)(writeStatement)
                  _ <- write("} ")
                } yield ()
            }
          } yield ()

        case JSReturn(value) =>
          for {
            _ <- write("return ")
            _ <- writeExprParen(value)
            _ <- write(";")
          } yield ()

        case JSBlockStatement(body) =>
          for {
            _ <- write("{")
            _ <- ZIO.foreach_(body)(writeStatement)
            _ <- write("}")
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
          writeBinding(JSBindingIdentifier(binding))

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
            _ <- ZIO.foreach_(bindings) { binding =>
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
            _ <- ZIO.foreach_(members) {
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
                  _ <- ZIO.foreach_(value)(writeStatement)
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

        case JSNumberInt(i) =>
          write(i.toString)

        case JSBoolean(true) => write("true")
        case JSBoolean(false) => write("false")

        case JSFunctionCall(function, args) =>
          for {
            _ <- writeExprParen(function)
            _ <- write("(")
            _ <- ZIO.foreach_(args) { arg =>
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
            _ <- ZIO.foreach_(args) { arg =>
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
            _ <- ZIO.foreach(name)(writeIdentifier)
            _ <- write("(")
            _ <- writeParameterList(parameters)
            _ <- write("){")
            _ <- ZIO.foreach_(body)(writeStatement)
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
            _ <- ZIO.foreach_(body)(writeStatement)
            _ <- write("}")
          } yield ()


        case JSArrayLiteral(values) =>
          for {
            _ <- write("[")
            _ <- ZIO.foreach_(values) { value =>
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

    def writeString(str: String): F[Unit] =
      write(StringHelpers.escapeJSString(str))

    def writeParameterList(params: JSFunctionParameterList): F[Unit] =
      params match {
        case JSFunctionEmptyParameterList => IO.unit
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
