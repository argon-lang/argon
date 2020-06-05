package dev.argon.backend.js

import cats.data.NonEmptyList

object JSDSL {

  trait ImportSyntax[+Stmt <: JSImportStatement] {
    def from(modulePath: String): Stmt
  }

  implicit def pairToJSProp(pair: (String, JSExpression)): JSObjectProperty =
    JSObjectProperty(pair._1, pair._2)

  implicit def stringToJSLiteral(str: String): JSString =
    JSString(str)

  implicit class IdSyntaxHelper(private val sc: StringContext) extends AnyVal {
    def id(args: Any*): JSIdentifier = JSIdentifier(sc.s(args: _*))
  }

  def get(name: String)(body: JSStatement*): JSObjectGetProperty =
    JSObjectGetProperty(name, body.toVector)

  def jsobj(members: JSObjectMember*): JSObjectLiteral = JSObjectLiteral(members.toVector)

  def jsarr(values: JSExpression*): JSArrayLiteral = JSArrayLiteral(values.toVector)

  def jsmodule(stmts: JSModuleStatement*): JSModule = JSModule(stmts.toVector)

  private def convertParams(parameters: List[JSIdentifier]): JSFunctionParameterList =
    parameters match {
      case Nil => JSFunctionEmptyParameterList
      case head :: tail => JSFunctionParameter(JSBindingIdentifier(head), convertParams(tail))
    }

  def jsfunction(name: Option[JSIdentifier])(parameters: JSIdentifier*)(body: JSStatement*): JSFunctionExpression =
    JSFunctionExpression(name, convertParams(parameters.toList), body.toVector)

  object import_* {
    def as(name: JSIdentifier): ImportSyntax[JSImportAllStatement] = new ImportSyntax[JSImportAllStatement] {
      override def from(modulePath: String): JSImportAllStatement =
        JSImportAllStatement(name, modulePath)
    }
  }

  def `import`(defaultExport: JSIdentifier): ImportSyntax[JSImportDefaultStatement] = new ImportSyntax[JSImportDefaultStatement] {
    override def from(modulePath: String): JSImportDefaultStatement =
      JSImportDefaultStatement(defaultExport, modulePath)
  }

  def `import`(defaultExport: JSIdentifier, `* as`: JSIdentifier): ImportSyntax[JSImportDefaultAndAllStatement] = new ImportSyntax[JSImportDefaultAndAllStatement] {
    override def from(modulePath: String): JSImportDefaultAndAllStatement =
      JSImportDefaultAndAllStatement(defaultExport, `* as`, modulePath)
  }

  object export {
    def default(expr: JSExpression): JSExportDefaultStatement =
      JSExportDefaultStatement(expr)
  }


  implicit class ExpressionExtensions(private val expr: JSExpression) extends AnyVal {
    def :=(value: JSExpression): JSAssignment =
      JSAssignment(expr, value)

    def prop(id: JSIdentifier): JSPropertyAccessDot =
      JSPropertyAccessDot(expr, id)

    def cprop(prop: JSExpression): JSPropertyAccessBracket =
      JSPropertyAccessBracket(expr, prop)

    def apply(args: JSExpression*): JSFunctionCall =
      JSFunctionCall(expr, args.toVector)
  }

  implicit class IdentifierExtensions(private val id: JSIdentifier) extends AnyVal {
    def ::=(value: JSExpression): JSDeclareInit =
      JSBindingIdentifier(id) ::= value
  }


  implicit class BindingExtensions(private val binding: JSBinding) extends AnyVal {
    def ::=(value: JSExpression): JSDeclareInit =
      JSDeclareInit(binding, value)
  }

  trait BindingSyntax {
    def ::=(value: JSExpression): JSDeclareInit
  }

  def const(decl: JSDeclaration, restDecl: JSDeclaration*): JSConst =
    JSConst(NonEmptyList(decl, restDecl.toList))

  def let(decl: JSDeclaration, restDecl: JSDeclaration*): JSLet =
    JSLet(NonEmptyList(decl, restDecl.toList))

}
