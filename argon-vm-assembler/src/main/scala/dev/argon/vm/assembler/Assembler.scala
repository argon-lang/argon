package dev.argon.vm.assembler

import dev.argon.vm.format
import dev.argon.vm.format.{Instruction, valueTypeTypeCanEqual}
import zio.*
import zio.stm.*

import java.io.ByteArrayOutputStream
import dev.argon.util.protobuf.enumCanEqual

import scala.collection.mutable


sealed abstract class Assembler private(val asmText: String) {
  import Assembler.MemberName

  protected val index: TRef[Int]


  protected val functions: TMap[String, format.Function]
  protected val functionNames: zio.stm.TRef[Seq[String]]
  protected val classes: TMap[String, format.Class]
  protected val classNames: TRef[Seq[String]]
  protected val fieldIndexes: TMap[MemberName, Long]
  protected val slotIndexes: TMap[MemberName, Long]

  private def parse: IO[ParseException, format.Program] =
    for
      _ <- parseTypes
      _ <- parseFunctions
      entrypoint <- ChunkParser.parseChunk(Map.empty)
      _ <- skipWhitespace
      index <- index.get.commit
      _ <- ZIO.fail(new ParseException("Unexpected remaining text: " + asmText.substring(index, index + 10)))
        .when(index < asmText.length)
      functionNames <- functionNames.get.commit
      orderedFunctions <- ZIO.foreach(functionNames) { name =>
        functions.get(name).commit.some.orElseFail(new ParseException("Unknown function: " + name))
      }
      classNames <- classNames.get.commit
      orderedClasses <- ZIO.foreach(classNames) { name =>
        classes.get(name).commit.some.orElseFail(new ParseException("Unknown class: " + name))
      }
    yield format.Program(
      globalTypes = Seq.empty,
      functions = orderedFunctions,
      classes = orderedClasses,
      entrypoint = entrypoint,
    )

  private def skipWhitespace: UIO[Unit] = {

    def impl(index: Int): Int =
      if index >= asmText.length then
        index
      else
        val codepoint = asmText.codePointAt(index)
        if Character.isWhitespace(codepoint) then
          impl(index + Character.charCount(codepoint))
        else
          index
        end if
      end if

    index.update(impl).commit
  }

  private def peekIdentifier: UIO[Option[String]] =
    def findIdentifierEnd(index: Int): Int =
      if index >= asmText.length then
        index
      else
        val codepoint = asmText.codePointAt(index)
        if isValidIdentifierCharacter(codepoint) then
          findIdentifierEnd(index + Character.charCount(codepoint))
        else
          index
        end if
      end if


    for
      _ <- skipWhitespace
      startIndex <- index.get.commit
      idEnd = findIdentifierEnd(startIndex)
    yield
      if idEnd > startIndex then
        Some(asmText.substring(startIndex, idEnd).nn)
      else
        None
  end peekIdentifier


  private def isValidIdentifierCharacter(codepoint: Int): Boolean =
    Character.isLetterOrDigit(codepoint) || codepoint == '_'

  private def skipIdentifier(id: String): UIO[Unit] =
    index.update(_ + id.length).commit

  private def parseIdentifier: IO[ParseException, String] =
    for
      id <- peekIdentifier
      id <- ZIO.fromEither(id.toRight { new ParseException("Expected identifier") })
      _ <- skipIdentifier(id)
    yield id

  private def tryParseSymbol(symbol: String): UIO[Boolean] =
    for
      _ <- skipWhitespace
      currentIndex <- index.get.commit
      result = asmText.startsWith(symbol, currentIndex)
      _ <- index.update(_ + symbol.length).commit.when(result)
    yield result

  private def parseSymbol(symbol: String): IO[ParseException, Unit] =
    ZIO.unlessZIO(tryParseSymbol(symbol))(
      index.get.commit.flatMap { index =>
        ZIO.fail {
          val found: String =
            if index < asmText.length then
              asmText.substring(index, index + 20).nn
            else
              "EOF"

          new ParseException("Expected '" + symbol + "', found '" + found + "'")
        }
      }
    ).unit

  private def parseTypes: IO[ParseException, Unit] =
    peekIdentifier.flatMap {
      case Some(id @ "CLASS") =>
        skipIdentifier(id) *> parseRemainingClass() *> parseTypes

      case _ => ZIO.unit
    }

  private def parseFunctions: IO[ParseException, Unit] =
    peekIdentifier.flatMap {
      case Some(id @ "FUNCTION") =>
        skipIdentifier(id) *> parseRemainingFunction() *> parseFunctions

      case Some(id @ "NATIVE") =>
        (
          for
            _ <- skipIdentifier(id)
            name <- parseIdentifier
            function = format.Function(format.Function.Func.Native(name))
            _ <- functions.put(name, function).commit
          yield ()
        ) *> parseFunctions

      case end =>
        ZIO.unit
    }

  private def parseRemainingFunction(): IO[ParseException, Unit] =
    for
      name <- parseIdentifier
      _ <- parseSymbol("(")
      variableIndexes <- TMap.empty[String, Int].commit
      parameterTypes <- {
        def parseRemainingParams(acc: Seq[format.ValueType]): IO[ParseException, Seq[format.ValueType]] =
          ZIO.ifZIO(tryParseSymbol(","))(
            onFalse = ZIO.succeed(acc),
            onTrue = {
              val nextParam =
                for
                  paramName <- parseIdentifier
                  _ <- parseSymbol(":")
                  t <- parseType
                  _ <- variableIndexes.size.flatMap(variableIndexes.put(paramName, _)).commit
                yield t
              nextParam.flatMap { t => parseRemainingParams(acc :+ t) }
            }
          )

        def parseParams: IO[ParseException, Seq[format.ValueType]] =
          peekIdentifier.flatMap {
            case Some(paramName) =>
              for
                _ <- skipIdentifier(paramName)
                _ <- parseSymbol(":")
                t <- parseType
                _ <- variableIndexes.size.flatMap(variableIndexes.put(paramName, _)).commit
                params <- parseRemainingParams(Seq(t))
              yield params


            case None => ZIO.succeed(Seq.empty)
          }

        parseParams
      }
      variableIndexes <- variableIndexes.toMap.commit

      _ <- parseSymbol(")")
      _ <- parseSymbol(":")
      returnType <- parseType
      body <- ChunkParser.parseChunk(variableIndexes)

      endId <- parseIdentifier

      _ <- ZIO.fail(new ParseException()).when(endId != "END")

      function = format.Function(format.Function.Func.Bytecode(format.BytecodeFunction(
        parameterTypes = parameterTypes,
        returnType = returnType,
        body = body,
      )))
      _ <- functions.put(name, function).commit
    yield ()

  private def parseRemainingClass(): IO[ParseException, Unit] =
    for
      name <- parseIdentifier

      baseClassId <- ZIO.ifZIO(tryParseSymbol(":"))(
        onTrue = parseIdentifier.flatMap(getClassIndex).asSome,
        onFalse = ZIO.none,
      )

      classDecl <- TRef.makeCommit(format.Class(
        baseClassId = baseClassId,
        fields = Seq.empty,
        slots = Seq.empty,
        implementations = Seq.empty,
      ))

      _ <- {
        def readField: IO[ParseException, Unit] =
          for
            fieldName <- parseIdentifier
            _ <- parseSymbol(":")
            t <- parseType
            fieldIndex <- classDecl.modify { classDecl =>
              (
                classDecl.fields.size,
                classDecl.copy(fields = classDecl.fields :+ format.Field(`type` = t))
              )
            }.commit
            _ <- fieldIndexes.put(MemberName(name, fieldName), fieldIndex).commit
          yield ()

        def readSlot: IO[ParseException, Unit] =
          for
            slotName <- parseIdentifier
            _ <- parseSymbol("(")
            params <- ZIO.ifZIO(tryParseSymbol(")"))(
              onTrue = ZIO.succeed(Seq.empty),
              onFalse =
                for
                  t <- parseType
                  params <- {
                    def loop(params: Seq[format.ValueType]): IO[ParseException, Seq[format.ValueType]] =
                      ZIO.ifZIO(tryParseSymbol(","))(
                        onFalse = ZIO.succeed(params),
                        onTrue = parseType.flatMap { t => loop(params :+ t) },
                      )

                    loop(Seq(t))
                  }
                  _ <- parseSymbol(")")
                yield params
            )
            _ <- parseSymbol(":")
            returnType <- parseType
            slotIndex <- classDecl.modify { classDecl =>
              (
                classDecl.slots.size,
                classDecl.copy(slots = classDecl.slots :+ format.MethodSlot(parameterTypes = params, returnType = returnType))
              )
            }.commit
            _ <- slotIndexes.put(MemberName(name, slotName), slotIndex).commit
          yield ()

        def readImplClass: IO[ParseException, Unit] =
          for
            className <- parseIdentifier
            _ <- parseSymbol(".")
            memberName <- parseIdentifier
            _ <- parseSymbol("=")
            functionName <- parseIdentifier

            declaringClass <- getClassIndex(className)
            slotIndex <- slotIndexes.get(MemberName(className, memberName)).commit
              .flatMap { res => ZIO.fromEither(res.toRight(new ParseException("Unknown slot"))) }
            functionIndex <- getFunctionIndex(functionName)

            _ <- classDecl.update { classDecl =>
              classDecl.copy(implementations = classDecl.implementations :+ format.MethodImplementation(
                declaringType = format.MethodImplementation.DeclaringType.DeclaringClassId(declaringClass),
                slotIndex = slotIndex,
                functionIndex = functionIndex,
              ))
            }.commit
          yield ()

        def classBody: IO[ParseException, Unit] =
          parseIdentifier.flatMap {
            case "END" => ZIO.unit
            case "FIELD" => readField *> classBody
            case "SLOT" => readSlot *> classBody
            case "IMPL_CLASS" => readImplClass *> classBody
            case _ => ZIO.fail(new ParseException())
          }

        classBody
      }

      classDecl <- classDecl.get.commit
      _ <- classes.put(name, classDecl).commit
    yield ()

  private def parseType: IO[ParseException, format.ValueType] =
    parseIdentifier.flatMap {
      case "INT8" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Int8)))
      case "INT16" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Int16)))
      case "INT32" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Int32)))
      case "INT64" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Int64)))
      case "FLOAT32" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Float32)))
      case "FLOAT64" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.Float64)))
      case "OBJECT_REFERENCE" => ZIO.succeed(format.ValueType(format.ValueType.Type.Simple(format.ValueTypeSimple.ObjectReference)))
      case "TUPLE" =>
        for
          _ <- parseSymbol("(")
          elements <- ZIO.ifZIO(tryParseSymbol(")"))(
            onTrue = ZIO.succeed(Seq.empty),
            onFalse =
              for
                t <- parseType
                elements <- {
                  def loop(elements: Seq[format.ValueType]): IO[ParseException, Seq[format.ValueType]] =
                    ZIO.ifZIO(tryParseSymbol(","))(
                      onFalse = ZIO.succeed(elements),
                      onTrue = parseType.flatMap { t => loop(elements :+ t) },
                    )

                  loop(Seq(t))
                }
              yield elements
          )
        yield format.ValueType(format.ValueType.Type.Tuple(format.ValueTypeTuple(elements)))

      case id => throw new ParseException("Unexpected type: " + id)
    }

  private abstract class ChunkParser {
    import ChunkParser.JumpTarget

    protected val variableIndexes: TMap[String, Int]

    protected val constants: TRef[Seq[format.ConstantValue]]
    protected val variableTypes: TRef[Seq[format.ValueType]]
    protected val os: ByteArrayOutputStream
    protected val labelNames: TRef[Seq[String]]
    protected val labelTargets: TMap[String, Int]

    def parseChunk: IO[ParseException, format.Chunk] =
      for
        _ <- parseInstructions
        constants <- constants.get.commit
        variableTypes <- variableTypes.get.commit
        labelNames <- labelNames.get.commit
        labels <- ZIO.foreach(labelNames) { labelName =>
          for
            target <- labelTargets.get(labelName).commit
            target <- ZIO.fromEither(target.toRight { ParseException(s"Unknown label: $labelName") })
          yield target.toLong
        }
        bytecode <- ZIO.succeed { os.toByteArray.nn }
      yield format.Chunk(
        constants = constants,
        variableTypes = variableTypes,
        labelTargets = labels,
        bytecode = Chunk.fromArray(bytecode)
      )

    private def parseInstructions: IO[ParseException, Unit] =
      peekIdentifier.flatMap {
        case None | Some("END") => ZIO.unit

        case Some(opName @ "LOCAL") =>
          (
            for
              _ <- skipIdentifier(opName)
              id <- parseIdentifier
              _ <- parseSymbol(":")
              t <- parseType
              _ <- variableTypes.update(_ :+ t).commit
              _ <- variableIndexes.size.flatMap(variableIndexes.put(id, _)).commit
            yield ()
          ).zipRight(parseInstructions)

        case Some(opName) =>
          skipIdentifier(opName)
            .zipRight {
              ZIO.ifZIO(tryParseSymbol(":"))(
                onTrue =
                  ZIO.succeed { os.size }
                    .flatMap { labelTargets.put(opName, _).commit }
                    .zipRight(parseInstructions),
                onFalse =
                  parseInstruction(opName)
                    .flatMap { insn => ZIO.succeed { insn.write(os) } }
                    .zipRight(parseInstructions)
              )
            }
      }

    private def parseInstruction(opName: String): IO[ParseException, Instruction] =
      val partial: PartialFunction[String, IO[ParseException, Instruction]] = {
        case "NOP" => ZIO.succeed(Instruction.NOP())
        case "POP" => ZIO.succeed(Instruction.POP())
        case "RETURN" => ZIO.succeed(Instruction.RETURN())
        case "CONSTANT" =>
          for
            t <- parseType
            constant <- parseConstant(t)
            index <- constants.modify { constants =>
              (
                constants.size,
                constants :+ constant
              )
            }.commit
          yield Instruction.CONSTANT(index)
        case "NEGATE" => ZIO.succeed(Instruction.NEGATE())
        case "ADD" => ZIO.succeed(Instruction.ADD())
        case "SUBTRACT" => ZIO.succeed(Instruction.SUBTRACT())
        case "MULTIPLY" => ZIO.succeed(Instruction.MULTIPLY())
        case "DIVIDE" => ZIO.succeed(Instruction.DIVIDE())
        case "DIVIDE_UN" => ZIO.succeed(Instruction.DIVIDE_UN())
        case "CONSTANT_0_INT32" => ZIO.succeed(Instruction.CONSTANT_0_INT32())
        case "CONSTANT_1_INT32" => ZIO.succeed(Instruction.CONSTANT_1_INT32())
        case "CONSTANT_NULL" => ZIO.succeed(Instruction.CONSTANT_NULL())
        case "EQZ" => ZIO.succeed(Instruction.EQZ())
        case "NEZ" => ZIO.succeed(Instruction.NEZ())
        case "EQ" => ZIO.succeed(Instruction.EQ())
        case "NE" => ZIO.succeed(Instruction.NE())
        case "LT" => ZIO.succeed(Instruction.LT())
        case "LT_UN" => ZIO.succeed(Instruction.LT_UN())
        case "GT" => ZIO.succeed(Instruction.GT())
        case "GT_UN" => ZIO.succeed(Instruction.GT_UN())
        case "JMP16" => parseJump(Instruction.JMP.apply)
        case "JZ16" => parseJump(Instruction.JZ.apply)
        case "JNZ16" => parseJump(Instruction.JNZ.apply)
        case "CALL" => parseCall(Instruction.CALL.apply)
        case "RETURN_CALL" => parseCall(Instruction.RETURN_CALL.apply)
        case "LD_LOCAL" => parseVariableInstruction(Instruction.LD_LOCAL.apply)
        case "ST_LOCAL" => parseVariableInstruction(Instruction.ST_LOCAL.apply)
        case "NEW" =>
          for
            className <- parseIdentifier
            index <- getClassIndex(className)
          yield Instruction.NEW(index)
        case "LD_FIELD" => parseFieldInstruction(Instruction.LD_FIELD.apply)
        case "ST_FIELD" => parseFieldInstruction(Instruction.ST_FIELD.apply)
        case "CALL_CLASS" => parseCallClass(Instruction.CALL_CLASS.apply)
        case "RETURN_CALL_CLASS" => parseCallClass(Instruction.RETURN_CALL_CLASS.apply)
      }

      ZIO.fromEither(partial.lift(opName).toRight(new ParseException(s"Unknown opcode: $opName"))).flatten
    end parseInstruction

    private def parseConstant(t: format.ValueType): IO[ParseException, format.ConstantValue] =
      t.`type` match {
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Int8) => parseIntLiteral(_.toByte, _.toInt, format.ConstantValue.Value.Int8.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Int16) => parseIntLiteral(_.toShort, _.toInt, format.ConstantValue.Value.Int16.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Int32) => parseIntLiteral(_.toInt, identity, format.ConstantValue.Value.Int32.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Int64) => parseIntLiteral(_.toLong, identity, format.ConstantValue.Value.Int64.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Float32) => parseFloatLiteral(_.toFloat, format.ConstantValue.Value.Float32.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.Float64) => parseFloatLiteral(_.toDouble, format.ConstantValue.Value.Float64.apply)
        case format.ValueType.Type.Simple(format.ValueTypeSimple.ObjectReference) =>
          parseIdentifier.flatMap {
            case "STRING" =>
              for
                value <- parseStringLiteral
              yield format.ConstantValue(format.ConstantValue.Value.StringLiteral(value))

            case _ =>
              ZIO.fail(new ParseException())
          }
        case format.ValueType.Type.Tuple(format.ValueTypeTuple(elements)) =>
          ZIO.foreach(elements)(parseConstant)
            .map { constants => format.ConstantValue(format.ConstantValue.Value.Tuple(format.TupleValue(constants))) }

        case format.ValueType.Type.Empty | format.ValueType.Type.Simple(format.ValueTypeSimple.Unrecognized(_)) =>
          ZIO.fail(new ParseException())
      }

    private def parseIntLiteral[T, U](parse: String => T, widen: T => U, create: U => format.ConstantValue.Value): IO[ParseException, format.ConstantValue] =
      for
        _ <- skipWhitespace
        startIndex <- index.get.commit
        _ <- tryParseSymbol("-")
        endIndex <- {
          def parseDigits(index: Int): Int =
            if index >= asmText.length then
              index
            else
              val codepoint = asmText.codePointAt(index)
              if Character.isDigit(codepoint) then
                parseDigits(index + Character.charCount(codepoint))
              else
                index
              end if
            end if

          index.updateAndGet(parseDigits).commit
        }
        _ <- ZIO.fail(new ParseException()).when(endIndex <= startIndex)
        value <- ZIO.attempt { parse(asmText.substring(startIndex, endIndex).nn) }
          .refineOrDie { case _: NumberFormatException => new ParseException() }

      yield format.ConstantValue(create(widen(value)))

    private def parseFloatLiteral[T](parse: String => T, create: T => format.ConstantValue.Value): IO[ParseException, format.ConstantValue] =
      for
        _ <- skipWhitespace
        startIndex <- index.get.commit
        _ <- tryParseSymbol("-")
        endIndex <- {
          def parseFloat(index: Int): Int =
            if index >= asmText.length then
              index
            else
              val codepoint = asmText.codePointAt(index)
              if isValidFloatChar(codepoint) then
                parseFloat(index + Character.charCount(codepoint))
              else
                index
              end if
            end if

          index.updateAndGet(parseFloat).commit
        }
        _ <- ZIO.fail(new ParseException()).when(endIndex <= startIndex)
        value <- ZIO.attempt {
          parse(asmText.substring(startIndex, endIndex).nn)
        }
          .refineOrDie { case _: NumberFormatException => new ParseException() }
      yield format.ConstantValue(create(value))

    private def isValidFloatChar(codepoint: Int): Boolean =
      Character.isDigit(codepoint) || codepoint == '.'

    private def parseStringLiteral: IO[ParseException, String] =
      for
        _ <- parseSymbol("\"")
        sb <- ZIO.succeed { new StringBuilder() }
        startIndex <- index.get.commit
        endIndex <- {
          def parseStringContent(index: Int): IO[ParseException, Int] =
            if index >= asmText.length then
              ZIO.fail(new ParseException())
            else
              val ch = asmText.charAt(index)
              if ch == '"' then
                ZIO.succeed(index + 1)
              else if ch == '\\' then
                if index + 1 < asmText.length then
                  ZIO.succeed { sb.append(asmText.charAt(index + 1)) } *> parseStringContent(index + 2)
                else
                  ZIO.fail(new ParseException())
              else
                ZIO.succeed { sb.append(ch) } *> parseStringContent(index + 1)
              end if
            end if

          parseStringContent(startIndex)
        }
        value <- ZIO.succeed { sb.toString }
        _ <- index.set(endIndex).commit
      yield value

    private def parseJump(create: Long => Instruction): IO[ParseException, Instruction] =
      for
        label <- parseIdentifier
        labelIndex <- getIndexFromName(label, labelNames)
      yield create(labelIndex)

    private def parseCall(create: Long => Instruction): IO[ParseException, Instruction] =
      for
        name <- parseIdentifier
        index <- getFunctionIndex(name)
      yield create(index)

    private def parseVariableInstruction(create: Long => Instruction): IO[ParseException, Instruction] =
      for
        name <- parseIdentifier
        index <- variableIndexes.get(name).commit
        index <- ZIO.fromEither(index.toRight { new ParseException(s"Unknown variable: $name") })
      yield create(index)

    private def parseFieldInstruction(create: (Long, Long) => Instruction): IO[ParseException, Instruction] =
      for
        className <- parseIdentifier
        _ <- parseSymbol(".")
        fieldName <- parseIdentifier
        classId <- getClassIndex(className)
        index <- fieldIndexes.get(MemberName(className, fieldName)).commit
        index <- ZIO.fromEither(index.toRight { new ParseException() })
      yield create(classId, index)

    private def parseCallClass(create: (Long, Long) => Instruction): IO[ParseException, Instruction] =
      for
        className <- parseIdentifier
        _ <- parseSymbol(".")
        slotName <- parseIdentifier
        classId <- getClassIndex(className)
        index <- slotIndexes.get(MemberName(className, slotName)).commit
        index <- ZIO.fromEither(index.toRight { new ParseException() })
      yield create(classId, index)
  }

  private object ChunkParser {
    final case class JumpTarget(offset: Int, targetLabel: String)

    def make(variableIndexes: Map[String, Int]): UIO[ChunkParser] =
      for
        varIndexes <- TMap.fromIterable(variableIndexes).commit
        consts <- TRef.makeCommit(Seq.empty[format.ConstantValue])
        varTypes <- TRef.makeCommit(Seq.empty[format.ValueType])
        baos <- ZIO.succeed {
          new ByteArrayOutputStream()
        }
        lblNames <- TRef.makeCommit(Seq.empty[String])
        lblTargets <- TMap.empty[String, Int].commit
      yield new ChunkParser {
        override protected val variableIndexes: TMap[String, Int] = varIndexes
        override protected val constants: TRef[Seq[format.ConstantValue]] = consts
        override protected val variableTypes: TRef[Seq[format.ValueType]] = varTypes
        override protected val os: ByteArrayOutputStream = baos
        override protected val labelNames: TRef[Seq[String]] = lblNames
        override protected val labelTargets: TMap[String, Int] = lblTargets
      }

    def parseChunk(variableIndexes: Map[String, Int]): IO[ParseException, format.Chunk] =
      for
        chunkParser <- make(variableIndexes)
        result <- chunkParser.parseChunk
      yield result

  }

  private def getFunctionIndex(name: String): UIO[Long] =
    getIndexFromName(name, functionNames)

  private def getClassIndex(name: String): UIO[Long] =
    getIndexFromName(name, classNames)

  private def getIndexFromName(name: String, names: TRef[Seq[String]]): UIO[Long] =
    names.get.flatMap { namesSeq =>
      val index = namesSeq.indexOf(name)
      if index >= 0 then
        ZSTM.succeed(index.toLong)
      else
        names.update(_ :+ name).as(namesSeq.size.toLong)
    }.commit
}



object Assembler {
  final case class MemberName(typeName: String, memberName: String)

  def parse(asmText: String): IO[ParseException, format.Program] =
    for
      idxs <- TRef.makeCommit(0)
      funcs <- TMap.empty[String, format.Function].commit
      funcNames <- TRef.makeCommit(Seq.empty[String])
      cls <- TMap.empty[String, format.Class].commit
      clsNames <- TRef.makeCommit(Seq.empty[String])
      fieldIdxs <- TMap.empty[MemberName, Long].commit
      slotIdxs <- TMap.empty[MemberName, Long].commit

      asm = new Assembler(asmText) {
        override protected val index: TRef[Int] = idxs
        override protected val functions: TMap[String, format.Function] = funcs
        override protected val functionNames: TRef[Seq[String]] = funcNames
        override protected val classes: TMap[String, format.Class] = cls
        override protected val classNames: TRef[Seq[String]] = clsNames
        override protected val fieldIndexes: TMap[MemberName, Long] = fieldIdxs
        override protected val slotIndexes: TMap[MemberName, Long] = slotIdxs
      }
      program <- asm.parse
    yield program
}
