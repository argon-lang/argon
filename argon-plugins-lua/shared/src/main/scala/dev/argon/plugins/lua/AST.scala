package dev.argon.plugins.lua

import zio.stream.*

import java.nio.charset.StandardCharsets

object AST {

  final case class Chunk(block: Block) {
    def toStream: UStream[String] = blockToStream(block)
  }

  final case class Block(statements: Seq[Stat])

  sealed trait Stat derives CanEqual
  final case class Assignment(vars: Seq[Var], exps: Seq[Exp]) extends Stat
  sealed trait FunctionCall extends Stat with PrefixExp
  final case class SimpleFunctionCall(func: PrefixExp, args: Seq[Exp]) extends FunctionCall
  final case class MethodCall(func: PrefixExp, name: String, args: Seq[Exp]) extends FunctionCall
  final case class Label(name: String) extends Stat
  case object Break extends Stat
  final case class Goto(name: String) extends Stat
  final case class Do(block: Block) extends Stat
  final case class While(condition: Exp, body: Block) extends Stat
  final case class Repeat(body: Block, condition: Exp) extends Stat
  final case class If(condition: Exp, body: Block, elseIfs: Seq[ElseIf], elseBody: Option[Block]) extends Stat
  final case class ElseIf(condition: Exp, body: Block)
  final case class NumericalFor(varName: String, init: Exp, limit: Exp, step: Option[Exp], body: Block) extends Stat
  final case class GenericFor(varNames: Seq[String], expList: Seq[Exp], body: Block) extends Stat
  final case class FunctionDeclaration(isLocal: Boolean, name: String, params: Seq[String], hasRest: Boolean, body: Block) extends Stat
  final case class LocalDeclaration(names: Seq[(String, Attrib)], values: Seq[Exp]) extends Stat
  enum Attrib derives CanEqual {
    case Empty
    case Const, Close
  }
  final case class Return(values: Seq[Exp]) extends Stat

  sealed trait Exp derives CanEqual
  sealed trait PrefixExp extends Exp
  sealed trait Var extends PrefixExp
  case object NilLiteral extends Exp
  case object FalseLiteral extends Exp
  case object TrueLiteral extends Exp
  final case class IntegerLiteral(value: Long) extends Exp
  final case class FloatLiteral(value: Double) extends Exp
  final case class StringLiteral(value: String) extends Exp
  case object RestExp extends Exp
  final case class FunctionDefinitionExp(params: Seq[String], hasRest: Boolean, body: Block) extends Exp
  final case class NameExp(name: String) extends Var
  final case class MemberAccessIndex(prefix: PrefixExp, member: Exp) extends Var
  final case class MemberAccessName(prefix: PrefixExp, name: String) extends Var
  final case class ParenExp(exp: Exp) extends PrefixExp
  final case class TableConstructor(fields: Seq[Field]) extends Exp
  enum Field derives CanEqual {
     case NamedWithExp(name: Exp, value: Exp)
     case NamedFixed(name: String, value: Exp)
     case Positional(value: Exp)
  }
  sealed trait OpExp extends Exp {
    val op: Op
  }
  sealed trait Op derives CanEqual {
    val precedence: Int
  }

  final case class BinOpExp(op: BinOp, a: Exp, b: Exp) extends OpExp
  enum BinOp(val text: String, val precedence: Int, val leftAssoc: Boolean) extends Op {
    case Add extends BinOp("+", 8, true)
    case Sub extends BinOp("-", 8, true)
    case Mul extends BinOp("*", 9, true)
    case Div extends BinOp("/", 9, true)
    case FloorDiv extends BinOp("//", 9, true)
    case Pow extends BinOp("^", 11, false)
    case Mod extends BinOp("%", 9, true)
    case BAnd extends BinOp("&", 5, true)
    case BXOr extends BinOp("~", 4, true)
    case BOr extends BinOp("|", 3, true)
    case ShiftLeft extends BinOp("<<", 6, true)
    case ShiftRight extends BinOp(">>", 6, true)
    case Concat extends BinOp("..", 7, false)
    case LT extends BinOp("<", 2, true)
    case LE extends BinOp("<=", 2, true)
    case GT extends BinOp(">", 2, true)
    case GE extends BinOp(">=", 2, true)
    case EQ extends BinOp("==", 2, true)
    case NE extends BinOp("~=", 2, true)
    case And extends BinOp("and", 1, true)
    case Or extends BinOp("or", 0, true)
  }
  final case class UnOpExp(op: UnOp, a: Exp) extends OpExp
  enum UnOp(val text: String) extends Op {
    override val precedence: Int = 10

    case Minus extends UnOp("-")
    case Not extends UnOp("not ")
    case Length extends UnOp("#")
    case BNot extends UnOp("~")
  }


  private def blockToStream(block: Block): UStream[String] =
    ZStream.fromIterable(block.statements).flatMap { stat =>
      statToStream(stat) ++ ZStream(";")
    }

  private def statToStream(stat: Stat): UStream[String] =
    stat match {
      case Assignment(vars, exps) =>
        ZStream.fromIterable(vars).flatMap(varToStream).intersperse(", ") ++
          ZStream(" = ")
        ZStream.fromIterable(exps).flatMap(expToStream).intersperse(", ")

      case functionCall: FunctionCall => expToStream(functionCall)

      case Label(name) =>
        ZStream("::", name, "::")

      case Break =>
        ZStream("break")

      case Goto(name) =>
        ZStream("goto ", name)

      case Do(block) =>
        ZStream("do ") ++ blockToStream(block) ++ ZStream(" end")

      case While(condition, body) =>
        ZStream("wihle ") ++
          expToStream(condition) ++
          ZStream(" do ") ++
          blockToStream(body) ++
          ZStream(" end")

      case Repeat(body, condition) =>
        ZStream("repeat ") ++
          blockToStream(body) ++
          ZStream(" until ") ++
          expToStream(condition)

      case If(condition, body, elseIfs, elseBody) =>
        ZStream("if ") ++
          expToStream(condition) ++
          ZStream(" then ") ++
          blockToStream(body) ++
          ZStream.fromIterable(elseIfs).flatMap {
            case ElseIf(condition, body) =>
              ZStream(" elseif ") ++
                expToStream(condition) ++
                ZStream(" then ") ++
                blockToStream(body)
          } ++
          ZStream.fromIterable(elseBody).flatMap { body =>
            ZStream(" else ") ++
              blockToStream(body) ++
              ZStream(" end")
          }

      case NumericalFor(varName, init, limit, step, body) =>
        ZStream("for ", varName, " = ") ++
          expToStream(init) ++
          ZStream(", ") ++
          expToStream(limit) ++
          ZStream.fromIterable(step).flatMap { s =>
            ZStream(", ") ++ expToStream(s)
          } ++
          ZStream(" do ") ++
          blockToStream(body) ++
          ZStream(" end")

      case GenericFor(varNames, expList, body) =>
        ZStream("for ") ++
          ZStream.fromIterable(varNames).intersperse(", ") ++
          ZStream(" in ") ++
          ZStream.fromIterable(expList).flatMap(expToStream).intersperse(", ") ++
          ZStream(" do ") ++
          blockToStream(body) ++
          ZStream(" end")

      case FunctionDeclaration(isLocal, name, params, hasRest, body) =>
        (if isLocal then ZStream("local ") else ZStream.empty) ++
          ZStream("function ",  name) ++
          writeFunctionBody(params, hasRest, body)

      case LocalDeclaration(names, values) =>
        ZStream("local ") ++
          ZStream.fromIterable(names).flatMap {
            case (name, Attrib.Empty) => ZStream(name)
            case (name, Attrib.Const) => ZStream(name, " const")
            case (name, Attrib.Close) => ZStream(name, " close")
          } ++
          (if values.nonEmpty then ZStream(" = ") else ZStream.empty) ++
          ZStream.fromIterable(values).flatMap(expToStream).intersperse(", ")

      case Return(Seq()) =>
        ZStream("return")

      case Return(values) =>
        ZStream("return ") ++ ZStream.fromIterable(values).flatMap(expToStream).intersperse(", ")
    }

  private def writeFunctionBody(params: Seq[String], hasRest: Boolean, body: Block): UStream[String] =
    ZStream("(") ++
      (ZStream.fromIterable(params) ++ (if hasRest then ZStream("...") else ZStream.empty)).intersperse(", ") ++
      ZStream(") ") ++
      blockToStream(body) ++
      ZStream(" end")

  private def isWhitelistedChar(ch: Char): Boolean =
    ch < 128 && (
      Character.isLetterOrDigit(ch) ||
        "!@#$%^&*()_+-=|`~[]{};:,./<>?".contains(ch)
    )

  private def expToStream(e: Exp): UStream[String] =
    e match {
      case NilLiteral => ZStream("nil")
      case FalseLiteral => ZStream("false")
      case TrueLiteral => ZStream("true")
      case IntegerLiteral(value) => ZStream("0x", value.toHexString)
      case FloatLiteral(value) => ZStream(java.lang.Double.toHexString(value).nn)
      case StringLiteral(value) =>
        val b = value.getBytes(StandardCharsets.UTF_8).nn
        ZStream.fromIterable(
          b.map { c =>
            val c2 = (c & 0xFF).toChar
            if isWhitelistedChar(c2) then
              c2.toString
            else
              "\\x%02X"
          }
        )

      case RestExp => ZStream("...")
      case FunctionDefinitionExp(params, hasRest, body) =>
        ZStream("function") ++ writeFunctionBody(params, hasRest, body)

      case p: PrefixExp => prefixToStream(p)
      case TableConstructor(fields) =>
        ZStream("{") ++
          ZStream.fromIterable(fields).flatMap {
            case Field.NamedWithExp(name, value) => ZStream("[") ++ expToStream(name) ++ ZStream("] = ") ++ expToStream(value) ++ ZStream(",")
            case Field.NamedFixed(name, value) => ZStream(name, " = ") ++ expToStream(value) ++ ZStream(",")
            case Field.Positional(value) => expToStream(value) ++ ZStream(",")
          } ++
          ZStream("}")

      case e: OpExp => opExpToStream(e)
    }

  private def varToStream(v: Var): UStream[String] =
    v match {
      case NameExp(name) => ZStream(name)
      case MemberAccessIndex(prefix, member) =>
        prefixToStream(prefix) ++
          ZStream("[") ++
          expToStream(member) ++
          ZStream("]")

      case MemberAccessName(prefix, name) =>
        prefixToStream(prefix) ++ ZStream(".", name)
    }

  private def prefixToStream(p: PrefixExp): UStream[String] =
    p match {
      case SimpleFunctionCall(f, Seq(s: StringLiteral)) =>
        expToStream(f) ++ ZStream(" ") ++ expToStream(s)

      case SimpleFunctionCall(f, Seq(t: TableConstructor)) =>
        expToStream(f) ++ ZStream(" ") ++ expToStream(t)

      case SimpleFunctionCall(f, args) =>
        expToStream(f) ++
          ZStream("(") ++
          ZStream.fromIterable(args).flatMap(expToStream).intersperse(", ") ++
          ZStream(")")

      case MethodCall(f, name, Seq(s: StringLiteral)) =>
        expToStream(f) ++ ZStream(":", name, " ") ++ expToStream(s)

      case MethodCall(f, name, Seq(t: TableConstructor)) =>
        expToStream(f) ++ ZStream(":", name, " ") ++ expToStream(t)

      case MethodCall(f, name, args) =>
        expToStream(f) ++
          ZStream(":", name, "(") ++
          ZStream.fromIterable(args).flatMap(expToStream).intersperse(", ") ++
          ZStream(")")

      case NameExp(name) =>
        ZStream(name)

      case MemberAccessIndex(prefix, member) =>
        prefixToStream(prefix) ++
          ZStream("[") ++
          expToStream(member) ++
          ZStream("]")

      case MemberAccessName(prefix, name) =>
        prefixToStream(prefix) ++ ZStream(".", name)

      case ParenExp(exp) =>
        ZStream("(") ++ expToStream(exp) ++ ZStream(")")
    }


  private def opExpToStream(e: OpExp): UStream[String] =
    e match {
      case BinOpExp(op, a, b) =>
        val aStream = opExpArgToStream(op, !op.leftAssoc, a)
        val bStream = opExpArgToStream(op, op.leftAssoc, b)
        aStream ++ ZStream(" ", op.text, " ") ++ bStream
        
      case UnOpExp(op, a) =>
        ZStream(op.text) ++ opExpArgToStream(op, true, a)
    }

  private def opExpArgToStream(op: Op, isAssocArg: Boolean, a: Exp): UStream[String] =
    a match {
      case a: OpExp if isAssocArg && a.op.precedence >= op.precedence => opExpToStream(a)
      case a: OpExp if !isAssocArg && a.op.precedence > op.precedence => opExpToStream(a)
      case a: OpExp => expToStream(ParenExp(a))
      case _ => expToStream(a)
    }

}
