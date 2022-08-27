package dev.argon.plugins.js.emit

import dev.argon.plugins.js.estree
import dev.argon.util.Nullable

import scala.annotation.targetName

private[emit] object JSExpr {


  trait ImportPartial {
    infix def from(source: String): estree.ImportDeclaration
  }

  def `import`(specifiers: estree.ImportSpecifier*): ImportPartial = source =>
    estree.ImportDeclaration(
      specifiers = specifiers,
      source = literal(source)
    )

  object import_* :
    infix def as(name: String): ImportPartial = source =>
      estree.ImportDeclaration(
        specifiers = Seq(estree.ImportNamespaceSpecifier(local = id(name))),
        source = literal(source)
      )
  end import_*


  def `return`(value: estree.Expression): estree.ReturnStatement =
    estree.ReturnStatement(argument = Nullable(value))

  val return_ : estree.ReturnStatement =
    estree.ReturnStatement(argument = Nullable(null))


  def id(name: String): estree.Identifier =
    estree.Identifier(name = name)

  def literal(s: String): estree.Literal =
    estree.Literal(value = Nullable(s))

  def literal(d: Double): estree.Literal =
    estree.Literal(value = Nullable(d))

  def literal(b: Boolean): estree.Literal =
    estree.Literal(value = Nullable(b))

  def literal(n: BigInt): estree.Literal =
    estree.Literal(value = Nullable(n), bigint = Some(n.toString))

  def literal(n: Null): estree.Literal =
    estree.Literal(value = Nullable(n))

  def obj(properties: estree.Property*): estree.ObjectExpression =
    estree.ObjectExpression(properties = properties)

  def block(statements: estree.Statement*): estree.BlockStatement =
    estree.BlockStatement(body = statements)

  def array(values: (estree.Expression | estree.SpreadElement | Null)*): estree.ArrayExpression =
    estree.ArrayExpression(elements = values.map(Nullable.apply))

  def exprStmt(expr: estree.Expression): estree.ExpressionStatement =
    estree.ExpressionStatement(expression = expr)

  trait ArrowPartial {
    @targetName("body")
    def ==> (body: estree.Expression): estree.ArrowFunctionExpression

    @targetName("body")
    def ==> (body: Seq[estree.Statement]): estree.ArrowFunctionExpression
  }

  object arrow:

    private def impl(isAsync: Boolean)(params: String*): ArrowPartial =
      new ArrowPartial:
        @targetName("body")
        override def ==>(body: estree.Expression): estree.ArrowFunctionExpression =
          estree.ArrowFunctionExpression(
            params = params.map(name => estree.Identifier(name = name)),
            body = body,
            async = isAsync,
          )

        @targetName("body")
        override def ==>(body: estree.Statement*): estree.ArrowFunctionExpression =
          estree.ArrowFunctionExpression(
            params = params.map(name => estree.Identifier(name = name)),
            body = estree.BlockStatement(body = body),
            async = isAsync,
          )
      end new

    def apply(params: String*): ArrowPartial = impl(isAsync = false)(params *)

    def async(params: String*): ArrowPartial = impl(isAsync = true)(params *)

  end arrow


  trait VariableDeclarationPartial {
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

  def let(name: String): VariableDeclarationPartial = value =>
    estree.VariableDeclaration(
      kind = "let",
      declarations = Seq(estree.VariableDeclarator(
        id = id(name),
        init = Nullable(value)
      )),
    )

  trait ExportVariableDeclarationPartial {
    def := (value: estree.Expression): estree.ExportNamedDeclaration
  }

  object `export` {
    infix def const(name: String): ExportVariableDeclarationPartial = value =>
      estree.ExportNamedDeclaration(
        declaration = Nullable(JSExpr.const(name) := value),
        specifiers = Seq(),
        source = Nullable(null)
      )

    def apply(declaration: estree.Declaration): estree.ExportNamedDeclaration =
      estree.ExportNamedDeclaration(
        declaration = Nullable(declaration),
        specifiers = Seq(),
        source = Nullable(null)
      )

  }


  trait PropertyPartial {
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


  final case class ImportOrExportSpecifier(original: estree.Identifier, renamed: estree.Identifier)

  extension (id: estree.Identifier)
    infix def as(renamed: estree.Identifier): ImportOrExportSpecifier =
      ImportOrExportSpecifier(id, renamed)

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

  def get(name: String)(body: estree.Statement*): estree.Property =
    estree.Property(
      kind = "get",
      key = id(name),
      value = estree.FunctionExpression(
        id = Nullable(null),
        params = Seq(),
        body = block(body*),
        generator = false,
        async = false,
      ),
      method = false,
      shorthand = false,
      computed = false,
    )

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
      estree.ImportSpecifier(local = x.renamed, imported = x.original)
  end given

  given Conversion[ImportOrExportSpecifier, estree.ExportSpecifier] with
    override def apply(x: ImportOrExportSpecifier): estree.ExportSpecifier =
      estree.ExportSpecifier(local = x.original, exported = x.renamed)
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
        computed = true,
        optional = false,
      )

    def call(args: estree.Expression*): estree.CallExpression =
      estree.CallExpression(
        callee = expr,
        arguments = args,
        optional = false,
      )

    def await: estree.AwaitExpression =
      estree.AwaitExpression(
        argument = expr,
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
