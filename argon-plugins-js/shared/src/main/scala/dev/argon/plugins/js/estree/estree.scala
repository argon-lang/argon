package dev.argon.plugins.js.estree

import dev.argon.plugins.js.JSValueCodec
import dev.argon.util.{*, given}

given variableDeclarationOrExpressionEncoder: JSValueCodec[VariableDeclaration | Expression] =
  JSValueCodec.union[VariableDeclaration, Expression]

given variableDeclarationOrPatternEncoder: JSValueCodec[VariableDeclaration | Pattern] =
  JSValueCodec.union[VariableDeclaration, Pattern]

given literalOrIdentifierEncoder: JSValueCodec[Literal | Identifier] =
  JSValueCodec.union[Literal, Identifier]

given statementOrModuleDeclarationEncoder: JSValueCodec[Statement | ModuleDeclaration] =
  JSValueCodec.union[Statement, ModuleDeclaration]

given expressionOrSuperEncoder: JSValueCodec[Expression | Super] =
  JSValueCodec.union[Expression, Super]

given blockStatementOrExpressionEncoder: JSValueCodec[BlockStatement | Expression] =
  JSValueCodec.union[BlockStatement, Expression]

given expressionOrPatternEncoder: JSValueCodec[Expression | Pattern] =
  JSValueCodec.union[Expression, Pattern]

given functionOrClassDeclarationEncoder: JSValueCodec[FunctionDeclaration | ClassDeclaration] =
  JSValueCodec.union[FunctionDeclaration, ClassDeclaration]

given expressionOrSpreadEncoder: JSValueCodec[Expression | SpreadElement] =
  JSValueCodec.union[Expression, SpreadElement]

given propertyOrSpreadEncoder: JSValueCodec[Property | SpreadElement] =
  JSValueCodec.union[Property, SpreadElement]

given propertyOrRestElementEncoder: JSValueCodec[Property | RestElement] =
  JSValueCodec.union[Property, RestElement]

given expressionOrPrivateIdentifierEncoder: JSValueCodec[Expression | PrivateIdentifier] =
  JSValueCodec.union[Expression, PrivateIdentifier]

given literalValueEncoder: JSValueCodec[String | Boolean | Double | BigInt] =
  given l1: JSValueCodec[Double | BigInt] = JSValueCodec.union[Double, BigInt]
  given l2: JSValueCodec[Boolean | Double | BigInt] = JSValueCodec.union[Boolean, Double | BigInt]
  JSValueCodec.union[String, Boolean | Double | BigInt]
end literalValueEncoder


sealed trait Node derives JSValueCodec {
  val `type`: String
  val loc: Nullable[SourceLocation]
}


final case class SourceLocation
(
  source: Nullable[String],
  start: Position,
  end: Position,
) derives JSValueCodec

final case class Position
(
  line: Int,
  column: Int,
) derives JSValueCodec



sealed trait Statement extends Node derives JSValueCodec


final case class Program
(
  `type`: "Program" = ("Program" : "Program"),
  loc: Nullable[SourceLocation] = Nullable(null),
  sourceType: "script" | "module",
  body: Seq[Statement | ModuleDeclaration],
) extends Node derives JSValueCodec


final case class ExpressionStatement
(
  `type`: "ExpressionStatement" = ("ExpressionStatement" : "ExpressionStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  expression: Expression,
  directive: Option[String] = None,
) extends Statement derives JSValueCodec

final case class BlockStatement
(
  `type`: "BlockStatement" = ("BlockStatement" : "BlockStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Seq[Statement],
) extends Statement derives JSValueCodec

final case class StaticBlock
(
  `type`: "StaticBlock" = ("StaticBlock" : "StaticBlock"),
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Statement,
) extends Statement derives JSValueCodec

final case class EmptyStatement
(
  `type`: "EmptyStatement" = ("EmptyStatement" : "EmptyStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Statement derives JSValueCodec

final case class DebuggerStatement
(
  `type`: "DebuggerStatement" = ("DebuggerStatement" : "DebuggerStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Statement derives JSValueCodec

final case class WithStatement
(
  `type`: "WithStatement" = ("WithStatement" : "WithStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  `object`: Expression,
  body: Statement,
) extends Statement derives JSValueCodec

final case class ReturnStatement
(
  `type`: "ReturnStatement" = ("ReturnStatement" : "ReturnStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
) extends Statement derives JSValueCodec

final case class LabeledStatement
(
  `type`: "LabeledStatement" = ("LabeledStatement" : "LabeledStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Identifier,
  body: Statement,
) extends Statement derives JSValueCodec

final case class BreakStatement
(
  `type`: "BreakStatement" = ("BreakStatement" : "BreakStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Nullable[Identifier],
) extends Statement derives JSValueCodec

final case class ContinueStatement
(
  `type`: "ContinueStatement" = ("ContinueStatement" : "ContinueStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Nullable[Identifier],
) extends Statement derives JSValueCodec

final case class IfStatement
(
  `type`: "IfStatement" = ("IfStatement" : "IfStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  consequent: Statement,
  alternate: Nullable[Statement],
) extends Statement derives JSValueCodec

final case class SwitchStatement
(
  `type`: "SwitchStatement" = ("SwitchStatement" : "SwitchStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  discriminant: Expression,
  cases: Seq[SwitchCase],
) extends Statement derives JSValueCodec

final case class SwitchCase
(
  `type`: "SwitchCase" = ("SwitchCase" : "SwitchCase"),
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Nullable[Expression],
  consequent: Seq[Statement],
) extends Node derives JSValueCodec

final case class ThrowStatement
(
  `type`: "ThrowStatement" = ("ThrowStatement" : "ThrowStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
) extends Statement derives JSValueCodec

final case class TryStatement
(
  `type`: "TryStatement" = ("TryStatement" : "TryStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  block: BlockStatement,
  handler: Nullable[CatchClause],
  finalizer: Nullable[BlockStatement],
) extends Statement derives JSValueCodec

final case class CatchClause
(
  `type`: "CatchClause" = ("CatchClause" : "CatchClause"),
  loc: Nullable[SourceLocation] = Nullable(null),
  param: Nullable[Pattern],
  body: BlockStatement,
) extends Node derives JSValueCodec

final case class WhileStatement
(
  `type`: "WhileStatement" = ("WhileStatement" : "WhileStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  body: Statement,
) extends Statement derives JSValueCodec

final case class DoWhileStatement
(
  `type`: "DoWhileStatement" = ("DoWhileStatement" : "DoWhileStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  body: Statement,
) extends Statement derives JSValueCodec

final case class ForStatement
(
  `type`: "ForStatement" = ("ForStatement" : "ForStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  init: Nullable[VariableDeclaration | Expression],
  test: Nullable[Expression],
  update: Nullable[Expression],
  body: Statement,
) extends Statement derives JSValueCodec

final case class ForInStatement
(
  `type`: "ForInStatement" = ("ForInStatement" : "ForInStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Nullable[VariableDeclaration | Pattern],
  right: Nullable[Expression],
  body: Statement,
) extends Statement derives JSValueCodec

final case class ForOfStatement
(
  `type`: "ForOfStatement" = ("ForOfStatement" : "ForOfStatement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Nullable[VariableDeclaration | Pattern],
  right: Nullable[Expression],
  body: Statement,
  await: Boolean,
) extends Statement derives JSValueCodec


sealed trait Declaration extends Statement derives JSValueCodec

final case class FunctionDeclaration
(
  `type`: "FunctionDeclaration" = ("FunctionDeclaration" : "FunctionDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  params: Seq[Pattern],
  body: BlockStatement,
  expression: Boolean,
  directive: Option[String],
  generator: Boolean,
  async: Boolean,
) extends Declaration derives JSValueCodec

final case class VariableDeclaration
(
  `type`: "VariableDeclaration" = ("VariableDeclaration" : "VariableDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  declarations: Seq[VariableDeclarator],
  kind: "var" | "let" | "const",
) extends Declaration derives JSValueCodec

final case class VariableDeclarator
(
  `type`: "VariableDeclarator" = ("VariableDeclarator" : "VariableDeclarator"),
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Pattern,
  init: Nullable[Expression],
) extends Node derives JSValueCodec

final case class ClassDeclaration
(
  `type`: "ClassDeclaration" = ("ClassDeclaration" : "ClassDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  superClass: Nullable[Expression],
  body: ClassBody,
) extends Declaration derives JSValueCodec




sealed trait Expression extends Node derives JSValueCodec

sealed trait Pattern extends Node derives JSValueCodec



final case class Identifier
(
  `type`: "Identifier" = ("Identifier" : "Identifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  name: String,
) extends Expression with Pattern derives JSValueCodec

final case class Literal
(
  `type`: "Literal" = ("Literal" : "Literal"),
  loc: Nullable[SourceLocation] = Nullable(null),
  value: Nullable[String | Boolean | Double | BigInt],
  regex: Option[RegExpLiteralOptions] = None,
  bigint: Option[String] = None,
) extends Expression with Pattern derives JSValueCodec



final case class RegExpLiteralOptions(pattern: String, flags: String) derives JSValueCodec



final case class ThisExpression
(
  `type`: "ThisExpression" = ("ThisExpression" : "ThisExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Expression with Pattern derives JSValueCodec

final case class ArrayExpression
(
  `type`: "ArrayExpression" = ("ArrayExpression" : "ArrayExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Seq[Nullable[Expression | SpreadElement]],
) extends Expression with Pattern derives JSValueCodec

final case class ObjectExpression
(
  `type`: "ObjectExpression" = ("ObjectExpression" : "ObjectExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  properties: Seq[Property | SpreadElement],
) extends Expression with Pattern derives JSValueCodec

final case class Property
(
  `type`: "Property" = ("Property" : "Property"),
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Expression,
  value: Expression | Pattern,
  kind: "init" | "get" | "set",
  method: Boolean,
  shorthand: Boolean,
  computed: Boolean,
) extends Node with Pattern derives JSValueCodec

final case class FunctionExpression
(
  `type`: "FunctionExpression" = ("FunctionExpression" : "FunctionExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  params: Seq[Pattern],
  body: BlockStatement,
  generator: Boolean,
  async: Boolean,
) extends Expression derives JSValueCodec

final case class ArrowFunctionExpression
(
  `type`: "ArrowFunctionExpression" = ("ArrowFunctionExpression" : "ArrowFunctionExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  params: Seq[Pattern],
  body: BlockStatement | Expression,
  generator: false = false,
  async: Boolean,
) extends Expression derives JSValueCodec

final case class UnaryExpression
(
  `type`: "UnaryExpression" = ("UnaryExpression" : "UnaryExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: UnaryOperator,
  prefix: Boolean,
  argument: Expression,
) extends Expression derives JSValueCodec
type UnaryOperator = "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"

final case class UpdateExpression
(
  `type`: "UpdateExpression" = ("UpdateExpression" : "UpdateExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: UpdateOperator,
  argument: Expression,
  prefix: Boolean,
) extends Expression derives JSValueCodec
type UpdateOperator = "++" | "--"

final case class BinaryExpression
(
  `type`: "BinaryExpression" = ("BinaryExpression" : "BinaryExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: BinaryOperator,
  left: Expression | PrivateIdentifier,
  right: Expression,
) extends Expression derives JSValueCodec
type BinaryOperator = "==" | "!=" | "===" | "!=="
  | "<" | "<=" | ">" | ">="
  | "<<" | ">>" | ">>>"
  | "+" | "-" | "*" | "/" | "%"
  | "|" | "^" | "&" | "in"
  | "instanceof"
  | "**"

final case class AssignmentExpression
(
  `type`: "AssignmentExpression" = ("AssignmentExpression" : "AssignmentExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: AssignmentOperator,
  left: Pattern,
  right: Expression,
) extends Expression derives JSValueCodec
type AssignmentOperator = "=" | "+=" | "-=" | "*=" | "/=" | "%="
  | "<<=" | ">>=" | ">>>="
  | "|=" | "^=" | "&="
  | "**="
  | "||=" | "&&=" | "??="

final case class LogicalExpression
(
  `type`: "LogicalExpression" = ("LogicalExpression" : "LogicalExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: LogicalOperator,
  left: Expression,
  right: Expression,
) extends Expression derives JSValueCodec
type LogicalOperator = "||" | "&&" | "??"

final case class MemberExpression
(
  `type`: "MemberExpression" = ("MemberExpression" : "MemberExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  `object`: Expression | Super,
  property: Expression | PrivateIdentifier,
  computed: Boolean,
  optional: Boolean,
) extends Expression with Pattern with ChainElement derives JSValueCodec

final case class ConditionalExpression
(
  `type`: "ConditionalExpression" = ("ConditionalExpression" : "ConditionalExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  alternate: Expression,
  consequent: Expression,
) extends Expression derives JSValueCodec

final case class Super
(
  `type`: "Super" = ("Super" : "Super"),
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Node derives JSValueCodec

final case class CallExpression
(
  `type`: "CallExpression" = ("CallExpression" : "CallExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  callee: Expression | Super,
  arguments: Seq[Expression | SpreadElement],
  optional: Boolean,
) extends Expression with ChainElement derives JSValueCodec

final case class NewExpression
(
  `type`: "NewExpression" = ("NewExpression" : "NewExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  callee: Expression,
  arguments: Seq[Expression | SpreadElement],
) extends Expression derives JSValueCodec

final case class SequenceExpression
(
  `type`: "SequenceExpression" = ("SequenceExpression" : "SequenceExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  expressions: Seq[Expression],
) extends Expression derives JSValueCodec

final case class YieldExpression
(
  `type`: "YieldExpression" = ("YieldExpression" : "YieldExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
  delegate: Boolean,
) extends Expression derives JSValueCodec

final case class TemplateLiteral
(
  `type`: "TemplateLiteral" = ("TemplateLiteral" : "TemplateLiteral"),
  loc: Nullable[SourceLocation] = Nullable(null),
  quasis: Seq[TemplateElement],
  expressions: Seq[Expression],
) extends Expression derives JSValueCodec

final case class TaggedTemplateExpression
(
  `type`: "TaggedTemplateExpression" = ("TaggedTemplateExpression" : "TaggedTemplateExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  tag: Expression,
  quasi: TemplateLiteral,
) extends Expression derives JSValueCodec

final case class TemplateElement
(
  `type`: "TemplateElement" = ("TemplateElement" : "TemplateElement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  tail: Boolean,
  value: TemplateElementValue,
) extends Node derives JSValueCodec

final case class TemplateElementValue
(
  cooked: Nullable[String],
  raw: String,
) derives JSValueCodec

final case class ClassExpression
(
  `type`: "ClassExpression" = ("ClassExpression" : "ClassExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  superClass: Nullable[Expression],
  body: ClassBody,
) extends Expression derives JSValueCodec

final case class MetaProperty
(
  `type`: "MetaProperty" = ("MetaProperty" : "MetaProperty"),
  loc: Nullable[SourceLocation] = Nullable(null),
  meta: Identifier,
  property: Identifier,
) extends Expression derives JSValueCodec

final case class AwaitExpression
(
  `type`: "AwaitExpression" = ("AwaitExpression" : "AwaitExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Expression,
) extends Expression derives JSValueCodec

final case class ChainExpression
(
  `type`: "ChainExpression" = ("ChainExpression" : "ChainExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  expression: ChainElement,
) extends Expression derives JSValueCodec

sealed trait ChainElement extends Node derives JSValueCodec {
  val optional: Boolean
}

final case class ImportExpression
(
  `type`: "ImportExpression" = ("ImportExpression" : "ImportExpression"),
  loc: Nullable[SourceLocation] = Nullable(null),
  source: Expression,
) extends Expression derives JSValueCodec




final case class SpreadElement
(
  `type`: "SpreadElement" = ("SpreadElement" : "SpreadElement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Expression,
) extends Node derives JSValueCodec




final case class ObjectPattern
(
  `type`: "ObjectPattern" = ("ObjectPattern" : "ObjectPattern"),
  loc: Nullable[SourceLocation] = Nullable(null),
  properties: Seq[Property | RestElement],
) extends Pattern derives JSValueCodec

final case class ArrayPattern
(
  `type`: "ArrayPattern" = ("ArrayPattern" : "ArrayPattern"),
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Seq[Nullable[Pattern]],
) extends Pattern derives JSValueCodec

final case class RestElement
(
  `type`: "RestElement" = ("RestElement" : "RestElement"),
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Pattern,
) extends Pattern derives JSValueCodec

final case class AssignmentPattern
(
  `type`: "AssignmentPattern" = ("AssignmentPattern" : "AssignmentPattern"),
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Pattern,
  right: Expression,
) extends Pattern derives JSValueCodec






final case class ClassBody
(
  `type`: "ClassBody" = ("ClassBody" : "ClassBody"),
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Seq[MethodDefinition | PropertyDefinition | StaticBlock],
) extends Node derives JSValueCodec
given classBodyBodyEncoder: JSValueCodec[MethodDefinition | PropertyDefinition | StaticBlock] =
  given JSValueCodec[PropertyDefinition | StaticBlock] = JSValueCodec.union[PropertyDefinition, StaticBlock]
  JSValueCodec.union[MethodDefinition, PropertyDefinition | StaticBlock]

final case class MethodDefinition
(
  `type`: "MethodDefinition" = ("MethodDefinition" : "MethodDefinition"),
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Expression | PrivateIdentifier,
  value: FunctionExpression,
  kind: "constructor" | "method" | "get" | "set",
  computed: Boolean,
  static: Boolean,
) extends Node derives JSValueCodec

final case class PropertyDefinition
(
  `type`: "PropertyDefinition" = ("PropertyDefinition" : "PropertyDefinition"),
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Expression | PrivateIdentifier,
  value: Nullable[Expression],
  computed: Boolean,
  static: Boolean,
) extends Node derives JSValueCodec

final case class PrivateIdentifier
(
  `type`: "PrivateIdentifier" = ("PrivateIdentifier" : "PrivateIdentifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  name: String,
) extends Node derives JSValueCodec






sealed trait ModuleDeclaration extends Node derives JSValueCodec

sealed trait ModuleSpecifier extends Node derives JSValueCodec {
  val local: Literal | Identifier
}


final case class ImportDeclaration
(
  `type`: "ImportDeclaration" = ("ImportDeclaration" : "ImportDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  specifiers: Seq[ModuleImportSpecifier],
  source: Literal,
) extends ModuleDeclaration derives JSValueCodec


sealed trait ModuleImportSpecifier extends ModuleSpecifier derives JSValueCodec


final case class ImportSpecifier
(
  `type`: "ImportSpecifier" = ("ImportSpecifier" : "ImportSpecifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
  imported: Identifier | Literal,
) extends ModuleImportSpecifier derives JSValueCodec

final case class ImportDefaultSpecifier
(
  `type`: "ImportDefaultSpecifier" = ("ImportDefaultSpecifier" : "ImportDefaultSpecifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
) extends ModuleImportSpecifier derives JSValueCodec

final case class ImportNamespaceSpecifier
(
  `type`: "ImportNamespaceSpecifier" = ("ImportNamespaceSpecifier" : "ImportNamespaceSpecifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
) extends ModuleImportSpecifier derives JSValueCodec


final case class ExportNamedDeclaration
(
  `type`: "ExportNamedDeclaration" = ("ExportNamedDeclaration" : "ExportNamedDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  declaration: Nullable[Declaration],
  specifiers: Seq[ExportSpecifier],
  source: Nullable[Literal],
) extends ModuleDeclaration derives JSValueCodec

final case class ExportSpecifier
(
  `type`: "ExportSpecifier" = ("ExportSpecifier" : "ExportSpecifier"),
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Literal | Identifier,
  exported: Literal | Identifier,
) extends ModuleImportSpecifier derives JSValueCodec

final case class ExportDefaultDeclaration
(
  `type`: "ExportDefaultDeclaration" = ("ExportDefaultDeclaration" : "ExportDefaultDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  declaration: FunctionDeclaration | ClassDeclaration,
) extends ModuleDeclaration derives JSValueCodec

final case class ExportAllDeclaration
(
  `type`: "ExportAllDeclaration" = ("ExportAllDeclaration" : "ExportAllDeclaration"),
  loc: Nullable[SourceLocation] = Nullable(null),
  source: Literal,
  exported: Nullable[Literal | Identifier],
) extends ModuleDeclaration derives JSValueCodec


