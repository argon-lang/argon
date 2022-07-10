package dev.argon.plugins.js.estree

import dev.argon.plugins.js.JSValueEncoder
import dev.argon.util.{*, given}

given variableDeclarationOrExpressionEncoder: JSValueEncoder[VariableDeclaration | Expression] =
  JSValueEncoder.union[VariableDeclaration, Expression]

given variableDeclarationOrPatternEncoder: JSValueEncoder[VariableDeclaration | Pattern] =
  JSValueEncoder.union[VariableDeclaration, Pattern]

given literalOrIdentifierEncoder: JSValueEncoder[Literal | Identifier] =
  JSValueEncoder.union[Literal, Identifier]

given statementOrModuleDeclarationEncoder: JSValueEncoder[Statement | ModuleDeclaration] =
  JSValueEncoder.union[Statement, ModuleDeclaration]

given expressionOrSuperEncoder: JSValueEncoder[Expression | Super] =
  JSValueEncoder.union[Expression, Super]

given blockStatementOrExpressionEncoder: JSValueEncoder[BlockStatement | Expression] =
  JSValueEncoder.union[BlockStatement, Expression]

given expressionOrPatternEncoder: JSValueEncoder[Expression | Pattern] =
  JSValueEncoder.union[Expression, Pattern]

given functionOrClassDeclarationEncoder: JSValueEncoder[FunctionDeclaration | ClassDeclaration] =
  JSValueEncoder.union[FunctionDeclaration, ClassDeclaration]

given expressionOrSpreadEncoder: JSValueEncoder[Expression | SpreadElement] =
  JSValueEncoder.union[Expression, SpreadElement]

given propertyOrRestElementEncoder: JSValueEncoder[Property | RestElement] =
  JSValueEncoder.union[Property, RestElement]

given expressionOrPrivateIdentifierEncoder: JSValueEncoder[Expression | PrivateIdentifier] =
  JSValueEncoder.union[Expression, PrivateIdentifier]

given literalValueEncoder: JSValueEncoder[String | Boolean | Double | BigInt] =
  given l1: JSValueEncoder[Double | BigInt] = JSValueEncoder.union[Double, BigInt]
  given l2: JSValueEncoder[Boolean | Double | BigInt] = JSValueEncoder.union[Boolean, Double | BigInt]
  JSValueEncoder.union[String, Boolean | Double | BigInt]
end literalValueEncoder


sealed trait Node derives JSValueEncoder {
  val `type`: String
  val loc: Nullable[SourceLocation]
}


final case class SourceLocation
(
  `type`: "SourceLocation" = "SourceLocation",
  source: Nullable[String],
  start: Position,
  end: Position,
) derives JSValueEncoder

final case class Position
(
  `type`: "Position" = "Position",
  source: Nullable[String],
  start: Int,
  end: Int,
) derives JSValueEncoder



sealed trait Statement extends Node derives JSValueEncoder


final case class Program
(
  `type`: "Program" = "Program",
  loc: Nullable[SourceLocation] = Nullable(null),
  sourceType: "script" | "module",
  body: Seq[Statement | ModuleDeclaration],
) extends Node derives JSValueEncoder


final case class ExpressionStatement
(
  `type`: "ExpressionStatement" = "ExpressionStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  expression: Expression,
  directive: Option[String],
) extends Statement derives JSValueEncoder

final case class BlockStatement
(
  `type`: "BlockStatement" = "BlockStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Statement,
) extends Statement derives JSValueEncoder

final case class StaticBlock
(
  `type`: "StaticBlock" = "StaticBlock",
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Statement,
) extends Statement derives JSValueEncoder

final case class EmptyStatement
(
  `type`: "EmptyStatement" = "EmptyStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Statement derives JSValueEncoder

final case class DebuggerStatement
(
  `type`: "DebuggerStatement" = "DebuggerStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Statement derives JSValueEncoder

final case class WithStatement
(
  `type`: "WithStatement" = "WithStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  `object`: Expression,
  body: Statement,
) extends Statement derives JSValueEncoder

final case class ReturnStatement
(
  `type`: "ReturnStatement" = "ReturnStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
) extends Statement derives JSValueEncoder

final case class LabeledStatement
(
  `type`: "LabeledStatement" = "LabeledStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Identifier,
  body: Statement,
) extends Statement derives JSValueEncoder

final case class BreakStatement
(
  `type`: "BreakStatement" = "BreakStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Nullable[Identifier],
) extends Statement derives JSValueEncoder

final case class ContinueStatement
(
  `type`: "ContinueStatement" = "ContinueStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  label: Nullable[Identifier],
) extends Statement derives JSValueEncoder

final case class IfStatement
(
  `type`: "IfStatement" = "IfStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  consequent: Statement,
  alternate: Nullable[Statement],
) extends Statement derives JSValueEncoder

final case class SwitchStatement
(
  `type`: "SwitchStatement" = "SwitchStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  discriminant: Expression,
  cases: Seq[SwitchCase],
) extends Statement derives JSValueEncoder

final case class SwitchCase
(
  `type`: "SwitchCase" = "SwitchCase",
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Nullable[Expression],
  consequent: Seq[Statement],
) extends Node derives JSValueEncoder

final case class ThrowStatement
(
  `type`: "ThrowStatement" = "ThrowStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
) extends Statement derives JSValueEncoder

final case class TryStatement
(
  `type`: "TryStatement" = "TryStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  block: BlockStatement,
  handler: Nullable[CatchClause],
  finalizer: Nullable[BlockStatement],
) extends Statement derives JSValueEncoder

final case class CatchClause
(
  `type`: "CatchClause" = "CatchClause",
  loc: Nullable[SourceLocation] = Nullable(null),
  param: Nullable[Pattern],
  body: BlockStatement,
) extends Node derives JSValueEncoder

final case class WhileStatement
(
  `type`: "WhileStatement" = "WhileStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  body: Statement,
) extends Statement derives JSValueEncoder

final case class DoWhileStatement
(
  `type`: "DoWhileStatement" = "DoWhileStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  body: Statement,
) extends Statement derives JSValueEncoder

final case class ForStatement
(
  `type`: "ForStatement" = "ForStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  init: Nullable[VariableDeclaration | Expression],
  test: Nullable[Expression],
  update: Nullable[Expression],
  body: Statement,
) extends Statement derives JSValueEncoder

final case class ForInStatement
(
  `type`: "ForInStatement" = "ForInStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Nullable[VariableDeclaration | Pattern],
  right: Nullable[Expression],
  body: Statement,
) extends Statement derives JSValueEncoder

final case class ForOfStatement
(
  `type`: "ForOfStatement" = "ForOfStatement",
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Nullable[VariableDeclaration | Pattern],
  right: Nullable[Expression],
  body: Statement,
  await: Boolean,
) extends Statement derives JSValueEncoder


sealed trait Declaration extends Node derives JSValueEncoder

final case class FunctionDeclaration
(
  `type`: "FunctionDeclaration" = "FunctionDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  params: Seq[Pattern],
  body: BlockStatement,
  expression: Expression,
  directive: Option[String],
  generator: Boolean,
  async: Boolean,
) extends Declaration derives JSValueEncoder

final case class VariableDeclaration
(
  `type`: "VariableDeclaration" = "VariableDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  declarations: Seq[VariableDeclarator],
  kind: "var" | "let" | "const",
) extends Declaration derives JSValueEncoder

final case class VariableDeclarator
(
  `type`: "VariableDeclarator" = "VariableDeclarator",
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Pattern,
  init: Nullable[Expression],
) extends Node derives JSValueEncoder

final case class ClassDeclaration
(
  `type`: "ClassDeclaration" = "ClassDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  superClass: Nullable[Expression],
  body: ClassBody,
) extends Declaration derives JSValueEncoder




sealed trait Expression extends Node derives JSValueEncoder

sealed trait Pattern extends Node derives JSValueEncoder



final case class Identifier
(
  `type`: "Identifier" = "Identifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  name: String,
) extends Expression with Pattern derives JSValueEncoder

final case class Literal
(
  `type`: "Literal" = "Literal",
  loc: Nullable[SourceLocation] = Nullable(null),
  value: Nullable[String | Boolean | Double | BigInt],
  regex: Option[RegExpLiteralOptions] = None,
  bigint: Option[String] = None,
) extends Expression with Pattern derives JSValueEncoder



final case class RegExpLiteralOptions(pattern: String, flags: String) derives JSValueEncoder



final case class ThisExpression
(
  `type`: "ThisExpression" = "ThisExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Expression with Pattern derives JSValueEncoder

final case class ArrayExpression
(
  `type`: "ArrayExpression" = "ArrayExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Seq[Nullable[Expression | SpreadElement]],
) extends Expression with Pattern derives JSValueEncoder

final case class ObjectExpression
(
  `type`: "ObjectExpression" = "ObjectExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Seq[Expression | SpreadElement],
) extends Expression with Pattern derives JSValueEncoder

final case class Property
(
  `type`: "Property" = "Property",
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Literal | Identifier,
  value: Expression | Pattern,
  kind: "init" | "get" | "set",
  method: Boolean,
  shorthand: Boolean,
  computed: Boolean,
) extends Node with Pattern derives JSValueEncoder

final case class FunctionExpression
(
  `type`: "FunctionExpression" = "FunctionExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  params: Seq[Pattern],
  body: BlockStatement,
  generator: Boolean,
  async: Boolean,
) extends Expression derives JSValueEncoder

final case class ArrowFunctionExpression
(
  `type`: "ArrowFunctionExpression" = "ArrowFunctionExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  params: Seq[Pattern],
  body: BlockStatement | Expression,
  generator: false = false,
) extends Expression derives JSValueEncoder

final case class UnaryExpression
(
  `type`: "UnaryExpression" = "UnaryExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: UnaryOperator,
  prefix: Boolean,
  argument: Expression,
) extends Expression derives JSValueEncoder
type UnaryOperator = "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"

final case class UpdateExpression
(
  `type`: "UpdateExpression" = "UpdateExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: UpdateOperator,
  argument: Expression,
  prefix: Boolean,
) extends Expression derives JSValueEncoder
type UpdateOperator = "++" | "--"

final case class BinaryExpression
(
  `type`: "BinaryExpression" = "BinaryExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: BinaryOperator,
  left: Expression | PrivateIdentifier,
  right: Expression,
) extends Expression derives JSValueEncoder
type BinaryOperator = "==" | "!=" | "===" | "!=="
  | "<" | "<=" | ">" | ">="
  | "<<" | ">>" | ">>>"
  | "+" | "-" | "*" | "/" | "%"
  | "|" | "^" | "&" | "in"
  | "instanceof"
  | "**"

final case class AssignmentExpression
(
  `type`: "AssignmentExpression" = "AssignmentExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: AssignmentOperator,
  left: Pattern,
  right: Expression,
) extends Expression derives JSValueEncoder
type AssignmentOperator = "=" | "+=" | "-=" | "*=" | "/=" | "%="
  | "<<=" | ">>=" | ">>>="
  | "|=" | "^=" | "&="
  | "**="
  | "||=" | "&&=" | "??="

final case class LogicalExpression
(
  `type`: "LogicalExpression" = "LogicalExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  operator: LogicalOperator,
  left: Expression,
  right: Expression,
) extends Expression derives JSValueEncoder
type LogicalOperator = "||" | "&&" | "??"

final case class MemberExpression
(
  `type`: "MemberExpression" = "MemberExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  `object`: Expression | Super,
  property: Expression | PrivateIdentifier,
  computed: Boolean,
  optional: Boolean,
) extends Expression with ChainElement derives JSValueEncoder

final case class ConditionalExpression
(
  `type`: "ConditionalExpression" = "ConditionalExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  test: Expression,
  alternate: Expression,
  consequent: Expression,
) extends Expression derives JSValueEncoder

final case class Super
(
  `type`: "Super" = "Super",
  loc: Nullable[SourceLocation] = Nullable(null),
) extends Node derives JSValueEncoder

final case class CallExpression
(
  `type`: "CallExpression" = "CallExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  callee: Expression | Super,
  arguments: Seq[Expression | SpreadElement],
  optional: Boolean,
) extends Expression with ChainElement derives JSValueEncoder

final case class NewExpression
(
  `type`: "NewExpression" = "NewExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  callee: Expression,
  arguments: Seq[Expression | SpreadElement],
) extends Expression derives JSValueEncoder

final case class SequenceExpression
(
  `type`: "SequenceExpression" = "SequenceExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  expressions: Seq[Expression],
) extends Expression derives JSValueEncoder

final case class YieldExpression
(
  `type`: "YieldExpression" = "YieldExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Nullable[Expression],
  delegate: Boolean,
) extends Expression derives JSValueEncoder

final case class TemplateLiteral
(
  `type`: "TemplateLiteral" = "TemplateLiteral",
  loc: Nullable[SourceLocation] = Nullable(null),
  quasis: Seq[TemplateElement],
  expressions: Seq[Expression],
) extends Expression derives JSValueEncoder

final case class TaggedTemplateExpression
(
  `type`: "TaggedTemplateExpression" = "TaggedTemplateExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  tag: Expression,
  quasi: TemplateLiteral,
) extends Expression derives JSValueEncoder

final case class TemplateElement
(
  `type`: "TemplateElement" = "TemplateElement",
  loc: Nullable[SourceLocation] = Nullable(null),
  tail: Boolean,
  value: TemplateElementValue,
) extends Node derives JSValueEncoder

final case class TemplateElementValue
(
  cooked: Nullable[String],
  raw: String,
) derives JSValueEncoder

final case class ClassExpression
(
  `type`: "ClassExpression" = "ClassExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  id: Nullable[Identifier],
  superClass: Nullable[Expression],
  body: ClassBody,
) extends Expression derives JSValueEncoder

final case class MetaProperty
(
  `type`: "MetaProperty" = "MetaProperty",
  loc: Nullable[SourceLocation] = Nullable(null),
  meta: Identifier,
  property: Identifier,
) extends Expression derives JSValueEncoder

final case class AwaitExpression
(
  `type`: "AwaitExpression" = "AwaitExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Expression,
) extends Expression derives JSValueEncoder

final case class ChainExpression
(
  `type`: "ChainExpression" = "ChainExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  expression: ChainElement,
) extends Expression derives JSValueEncoder

sealed trait ChainElement extends Node derives JSValueEncoder {
  val optional: Boolean
}

final case class ImportExpression
(
  `type`: "ImportExpression" = "ImportExpression",
  loc: Nullable[SourceLocation] = Nullable(null),
  source: Expression,
) extends Expression derives JSValueEncoder




final case class SpreadElement
(
  `type`: "SpreadElement" = "SpreadElement",
  loc: Nullable[SourceLocation] = Nullable(null),
  argument: Expression,
) extends Node derives JSValueEncoder




final case class ObjectPattern
(
  `type`: "ObjectPattern" = "ObjectPattern",
  loc: Nullable[SourceLocation] = Nullable(null),
  properties: Seq[Property | RestElement],
) extends Pattern derives JSValueEncoder

final case class ArrayPattern
(
  `type`: "ArrayPattern" = "ArrayPattern",
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Seq[Nullable[Pattern]],
) extends Pattern derives JSValueEncoder

final case class RestElement
(
  `type`: "RestElement" = "RestElement",
  loc: Nullable[SourceLocation] = Nullable(null),
  elements: Pattern,
) extends Pattern derives JSValueEncoder

final case class AssignmentPattern
(
  `type`: "AssignmentPattern" = "AssignmentPattern",
  loc: Nullable[SourceLocation] = Nullable(null),
  left: Pattern,
  right: Expression,
) extends Pattern derives JSValueEncoder






final case class ClassBody
(
  `type`: "ClassBody" = "ClassBody",
  loc: Nullable[SourceLocation] = Nullable(null),
  body: Seq[MethodDefinition | PropertyDefinition | StaticBlock],
) extends Node derives JSValueEncoder
given classBodyBodyEncoder: JSValueEncoder[MethodDefinition | PropertyDefinition | StaticBlock] =
  given JSValueEncoder[PropertyDefinition | StaticBlock] = JSValueEncoder.union[PropertyDefinition, StaticBlock]
  JSValueEncoder.union[MethodDefinition, PropertyDefinition | StaticBlock]

final case class MethodDefinition
(
  `type`: "MethodDefinition" = "MethodDefinition",
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Expression | PrivateIdentifier,
  value: FunctionExpression,
  kind: "constructor" | "method" | "get" | "set",
  computed: Boolean,
  static: Boolean,
) extends Node derives JSValueEncoder

final case class PropertyDefinition
(
  `type`: "PropertyDefinition" = "PropertyDefinition",
  loc: Nullable[SourceLocation] = Nullable(null),
  key: Expression | PrivateIdentifier,
  value: Nullable[Expression],
  computed: Boolean,
  static: Boolean,
) extends Node derives JSValueEncoder

final case class PrivateIdentifier
(
  `type`: "PrivateIdentifier" = "PrivateIdentifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  name: String,
) extends Node derives JSValueEncoder






sealed trait ModuleDeclaration extends Node derives JSValueEncoder

sealed trait ModuleSpecifier extends Node derives JSValueEncoder {
  val local: Literal | Identifier
}


final case class ImportDeclaration
(
  `type`: "ImportDeclaration" = "ImportDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  specifiers: Seq[ModuleImportSpecifier]
) extends ModuleDeclaration derives JSValueEncoder


sealed trait ModuleImportSpecifier extends ModuleSpecifier derives JSValueEncoder


final case class ImportSpecifier
(
  `type`: "ImportSpecifier" = "ImportSpecifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
  imported: Identifier | Literal,
) extends ModuleImportSpecifier derives JSValueEncoder

final case class ImportDefaultSpecifier
(
  `type`: "ImportDefaultSpecifier" = "ImportDefaultSpecifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
) extends ModuleImportSpecifier derives JSValueEncoder

final case class ImportNamespaceSpecifier
(
  `type`: "ImportNamespaceSpecifier" = "ImportNamespaceSpecifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Identifier,
) extends ModuleImportSpecifier derives JSValueEncoder


final case class ExportNamedDeclaration
(
  `type`: "ExportNamedDeclaration" = "ExportNamedDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  declaration: Nullable[Declaration],
  specifiers: Seq[ExportSpecifier],
  source: Nullable[Literal],
) extends ModuleDeclaration derives JSValueEncoder

final case class ExportSpecifier
(
  `type`: "ExportSpecifier" = "ExportSpecifier",
  loc: Nullable[SourceLocation] = Nullable(null),
  local: Literal | Identifier,
  exported: Literal | Identifier,
) extends ModuleImportSpecifier derives JSValueEncoder

final case class ExportDefaultDeclaration
(
  `type`: "ExportDefaultDeclaration" = "ExportDefaultDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  declaration: FunctionDeclaration | ClassDeclaration,
) extends ModuleDeclaration derives JSValueEncoder

final case class ExportAllDeclaration
(
  `type`: "ExportAllDeclaration" = "ExportAllDeclaration",
  loc: Nullable[SourceLocation] = Nullable(null),
  source: Literal,
  exported: Nullable[Literal | Identifier],
) extends ModuleDeclaration derives JSValueEncoder


