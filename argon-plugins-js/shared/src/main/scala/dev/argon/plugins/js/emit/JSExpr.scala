package dev.argon.plugins.js.emit

import dev.argon.plugins.js.estree
import dev.argon.util.Nullable

import scala.annotation.targetName

private[emit] object JSExpr {


  trait ImportPartial {
    def from(source: String): estree.ImportDeclaration
  }

  def `import`(specifiers: estree.ImportSpecifier*): ImportPartial = source =>
    estree.ImportDeclaration(
      specifiers = specifiers,
      source = literal(source)
    )


  def `return`(value: estree.Expression): estree.ReturnStatement =
    estree.ReturnStatement(argument = Nullable(value))

  val return_ : estree.ReturnStatement =
    estree.ReturnStatement(argument = Nullable(null))


  def id(name: String): estree.Identifier =
    estree.Identifier(name = name)

  def literal(s: String): estree.Literal =
    estree.Literal(value = Nullable(s))

  def literal(n: Null): estree.Literal =
    estree.Literal(value = Nullable(n))

  def obj(properties: estree.Property*): estree.ObjectExpression =
    estree.ObjectExpression(properties = properties)

  def block(statements: estree.Statement*): estree.BlockStatement =
    estree.BlockStatement(body = statements)

  trait ArrowPartial {
    @targetName("body")
    def ==> (body: estree.Expression): estree.ArrowFunctionExpression

    @targetName("body")
    def ==> (body: estree.Statement*): estree.ArrowFunctionExpression
  }

  def arrow(params: String*): ArrowPartial =
    new ArrowPartial:
      @targetName("body")
      override def ==>(body: estree.Expression): estree.ArrowFunctionExpression =
        estree.ArrowFunctionExpression(
          params = params.map(name => estree.Identifier(name = name)),
          body = body,
        )

      @targetName("body")
      override def ==>(body: estree.Statement*): estree.ArrowFunctionExpression =
        estree.ArrowFunctionExpression(
          params = params.map(name => estree.Identifier(name = name)),
          body = estree.BlockStatement(body = body),
        )

  trait VariableDeclarationPartial {
    @targetName("variableValue")
    def := (value: estree.Expression): estree.VariableDeclaration
  }

  def const(name: String): VariableDeclarationPartial = value =>
    estree.VariableDeclaration(
      kind = "const",
      declarations = Seq(estree.VariableDeclarator(
        id = id(name),
        init = Nullable(value)
      )),
    )

  trait ExportVariableDeclarationPartial {
    @targetName("variableValue")
    def := (value: estree.Expression): estree.ExportNamedDeclaration
  }

  object `export` {
    def const(name: String): ExportVariableDeclarationPartial = value =>
      estree.ExportNamedDeclaration(
        declaration = Nullable(JSExpr.const(name) := value),
        specifiers = Seq(),
        source = Nullable(null)
      )
  }


  trait PropertyPartial {
    @targetName("propertyValue")
    def :=(value: estree.Expression | estree.Pattern): estree.Property
  }

  def computed(expr: estree.Expression): PropertyPartial = value =>
    estree.Property(
      kind = "init",
      key = expr,
      value = value,
      method = false,
      shorthand = false,
      computed = true,
    )


  final case class ImportOrExportSpecifier(local: estree.Identifier, external: estree.Identifier)

  extension (id: estree.Identifier)
    def as(externalId: estree.Identifier): ImportOrExportSpecifier =
      ImportOrExportSpecifier(id, externalId)

    @targetName("propertyValue")
    def :=(value: estree.Expression | estree.Pattern): estree.Property =
      estree.Property(
        kind = "init",
        key = id,
        value = value,
        method = false,
        shorthand = false,
        computed = false,
      )
  end extension

  given Conversion[estree.Identifier, estree.ImportSpecifier] with
    override def apply(x: estree.Identifier): estree.ImportSpecifier =
      estree.ImportSpecifier(local = x, imported = x)
  end given

  given Conversion[estree.Expression, estree.ExpressionStatement] with
    override def apply(x: estree.Expression): estree.ExpressionStatement =
      estree.ExpressionStatement(expression = x)
  end given

  given Conversion[ImportOrExportSpecifier, estree.ImportSpecifier] with
    override def apply(x: ImportOrExportSpecifier): estree.ImportSpecifier =
      estree.ImportSpecifier(local = x.local, imported = x.external)
  end given

  given Conversion[ImportOrExportSpecifier, estree.ExportSpecifier] with
    override def apply(x: ImportOrExportSpecifier): estree.ExportSpecifier =
      estree.ExportSpecifier(local = x.local, exported = x.external)
  end given

  extension (expr: estree.Expression)

    def prop(name: String): estree.MemberExpression =
      estree.MemberExpression(
        `object` = expr,
        property = estree.Identifier(name = name),
        computed = false,
        optional = false,
      )

    def index(property: estree.Expression): estree.MemberExpression =
      estree.MemberExpression(
        `object` = expr,
        property = property,
        computed = false,
        optional = false,
      )

    def call(args: estree.Expression*): estree.CallExpression =
      estree.CallExpression(
        callee = expr,
        arguments = args,
        optional = false,
      )

  end extension

  extension (pattern: estree.Pattern)
    def ::= (value: estree.Expression): estree.AssignmentExpression =
      estree.AssignmentExpression(
        operator = "=",
        left = pattern,
        right = value,
      )
  end extension

}
