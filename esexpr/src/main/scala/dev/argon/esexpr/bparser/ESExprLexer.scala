package dev.argon.esexpr.bparser

import dev.argon.esexpr.{ESExpr, ESExprError}
import dev.argon.util.{FileOffset, Location, WithLocation, ZChannelUtil}
import zio.*
import zio.stream.*

import java.io.EOFException
import java.nio.charset.{CharacterCodingException, StandardCharsets}

object ESExprLexer {

  final case class UnexpectedTag(b: Byte) extends ESExprBinaryParseException("Unexpected tag")


  private enum VarIntTag derives CanEqual {
    case ConstructorStart, NonNegIntValue, NegIntValue, StringLengthValue, StringPoolValue, BytesLengthValue, KeywordArgument
  }

  private enum BufferTag derives CanEqual {
    case StringValue(len: Int)
    case BytesValue(len: Int)
    case Float32Value, Float64Value
  }

  private enum ReadState derives CanEqual {
    case Empty(dataOffset: FileOffset)
    case PartialVarInt(tag: VarIntTag, value: BigInt, startOffset: FileOffset, dataOffset: FileOffset)
    case PartialBuffer(tag: BufferTag, data: Chunk[Byte], startOffset: FileOffset, dataOffset: FileOffset)
  }

  private def readTokens(state: ReadState, fileName: Option[String], data: Chunk[Byte], prevTokens: Chunk[WithLocation[ESExprToken, FileOffset]]): IO[ESExprError, (ReadState, Chunk[WithLocation[ESExprToken, FileOffset]])] =
    state match {
      case ReadState.Empty(dataOffset) =>
        data match {
          case tag +: tail =>
            val tailOffset = FileOffset(dataOffset.offset + 1)
            if (tag & 0xE0) == 0xE0 then
              tag & 0xFF match {
                case 0xE0 => readTokens(ReadState.Empty(tailOffset), fileName, tail, prevTokens :+ WithLocation(ESExprToken.ConstructorEnd, Location(fileName, dataOffset, tailOffset)))
                case 0xE1 => readTokens(ReadState.Empty(tailOffset), fileName, tail, prevTokens :+ WithLocation(ESExprToken.BooleanValue(false), Location(fileName, dataOffset, tailOffset)))
                case 0xE2 => readTokens(ReadState.Empty(tailOffset), fileName, tail, prevTokens :+ WithLocation(ESExprToken.BooleanValue(true), Location(fileName, dataOffset, tailOffset)))
                case 0xE4 => readTokens(ReadState.PartialBuffer(BufferTag.Float32Value, Chunk.empty, dataOffset, tailOffset), fileName, tail, prevTokens)
                case 0xE5 => readTokens(ReadState.PartialBuffer(BufferTag.Float32Value, Chunk.empty, dataOffset, tailOffset), fileName, tail, prevTokens)
                case _ => ZIO.fail(UnexpectedTag(tag))
              }
            else
              val (tokenOpt, readState, remaining) = readVarIntTag(tag, fileName, dataOffset, tailOffset, tail)
              readTokens(readState, fileName, remaining, prevTokens ++ tokenOpt)
            end if

          case _ =>
            ZIO.succeed((state, prevTokens))
        }

      case ReadState.PartialVarInt(tag, value, startOffset, dataOffset) =>
        val (tokenOpt, readState, remaining) = readVarInt(tag, fileName, startOffset, value, dataOffset, data)
        readTokens(readState, fileName, remaining, prevTokens ++ tokenOpt)

      case ReadState.PartialBuffer(tag, prevData, startOffset, dataOffset) =>
        val (tokenOpt, readState, remaining) = readBuffer(tag, fileName, startOffset, prevData, dataOffset, data)
        readTokens(readState, fileName, remaining, prevTokens ++ tokenOpt)
    }

  private def readVarIntTag(tag: Byte, fileName: Option[String], startOffset: FileOffset, dataOffset: FileOffset, data: Chunk[Byte]): (Option[WithLocation[ESExprToken, FileOffset]], ReadState, Chunk[Byte]) =
    val varIntTag = (tag & 0xE0) match {
      case 0x00 => VarIntTag.ConstructorStart
      case 0x20 => VarIntTag.NonNegIntValue
      case 0x40 => VarIntTag.NegIntValue
      case 0x60 => VarIntTag.StringLengthValue
      case 0x80 => VarIntTag.StringPoolValue
      case 0xA0 => VarIntTag.BytesLengthValue
      case 0xC0 => VarIntTag.KeywordArgument
      case _ => throw new RuntimeException("Should not be reachable")
    }

    val initialValue: BigInt = tag & 0x0F

    if (tag & 0x10) == 0x10 then
      readVarInt(varIntTag, fileName, startOffset, initialValue, dataOffset, data)
    else
      val (tokenOpt, readState) = completeVarInt(varIntTag, fileName, startOffset, dataOffset, initialValue)
      (tokenOpt, readState, data)
    end if
  end readVarIntTag

  private def readVarInt(tag: VarIntTag, fileName: Option[String], startOffset: FileOffset, acc: BigInt, dataOffset: FileOffset, data: Chunk[Byte]): (Option[WithLocation[ESExprToken, FileOffset]], ReadState, Chunk[Byte]) =
    data match {
      case b +: tail =>
        val tailOffset = FileOffset(dataOffset.offset + 1)
        val updatedValue = (acc << 7) | (b & 0x7F)
        if (b & 0x80) == 0x80 then
          readVarInt(tag, fileName, startOffset, updatedValue, tailOffset, tail)
        else
          val (tokenOpt, readState) = completeVarInt(tag, fileName, startOffset, tailOffset, updatedValue)
          (tokenOpt, readState, tail)
        end if

      case _ =>
        (None, ReadState.PartialVarInt(tag, acc, startOffset, dataOffset), data)
    }

  private def completeVarInt(tag: VarIntTag, fileName: Option[String], startOffset: FileOffset, endOffset: FileOffset, value: BigInt): (Option[WithLocation[ESExprToken, FileOffset]], ReadState) =
    def tokenResult(token: ESExprToken): (Option[WithLocation[ESExprToken, FileOffset]], ReadState) =
      (Some(WithLocation(token, Location(fileName, startOffset, endOffset))), ReadState.Empty(endOffset))

    tag match {
      case VarIntTag.ConstructorStart => tokenResult(ESExprToken.ConstructorStart(value))
      case VarIntTag.NonNegIntValue => tokenResult(ESExprToken.IntValue(value))
      case VarIntTag.NegIntValue => tokenResult(ESExprToken.IntValue(-(value + 1)))
      case VarIntTag.StringPoolValue => tokenResult(ESExprToken.StringPoolValue(value))
      case VarIntTag.KeywordArgument => tokenResult(ESExprToken.KeywordArgument(value))

      case VarIntTag.StringLengthValue => (None, ReadState.PartialBuffer(BufferTag.StringValue(value.bigInteger.intValueExact()), Chunk.empty, startOffset, endOffset))
      case VarIntTag.BytesLengthValue => (None, ReadState.PartialBuffer(BufferTag.BytesValue(value.bigInteger.intValueExact()), Chunk.empty, startOffset, endOffset))
    }
  end completeVarInt

  private def readBuffer(tag: BufferTag, fileName: Option[String], startOffset: FileOffset, prevData: Chunk[Byte], dataOffset: FileOffset, newData: Chunk[Byte]): (Option[WithLocation[ESExprToken, FileOffset]], ReadState, Chunk[Byte]) =
    val data = prevData ++ newData

    val expectedLength = tag match {
      case BufferTag.StringValue(len) => len
      case BufferTag.BytesValue(len) => len
      case BufferTag.Float32Value => 4
      case BufferTag.Float64Value => 8
    }

    if data.length < expectedLength then
      val endOffset = FileOffset(dataOffset.offset + newData.size)
      (None, ReadState.PartialBuffer(tag, data, startOffset, endOffset), Chunk.empty)
    else
      val (tokenData, remaining) = data.splitAt(expectedLength)
      val endOffset = FileOffset(dataOffset.offset + expectedLength - prevData.size)

      val token = tag match {
        case BufferTag.StringValue(_) => ESExprToken.StringValue(new String(tokenData.toArray, StandardCharsets.UTF_8))
        case BufferTag.BytesValue(_) => ESExprToken.BytesValue(tokenData)
        case BufferTag.Float32Value =>
          val floatAsInt = ((tokenData.byte(3) & 0xFF) << 24) | ((tokenData.byte(2) & 0xFF) << 16) | ((tokenData.byte(1) & 0xFF) << 8) | (tokenData.byte(0) & 0xFF)
          ESExprToken.Float32Value(java.lang.Float.intBitsToFloat(floatAsInt))

        case BufferTag.Float64Value =>
          val floatAsLong = ((tokenData.byte(7) & 0xFFL) << 56) | ((tokenData.byte(6) & 0xFFL) << 48) | ((tokenData.byte(5) & 0xFFL) << 40) | ((tokenData.byte(4) & 0xFFL) << 32) |
            ((tokenData.byte(3) & 0xFFL) << 24) | ((tokenData.byte(2) & 0xFFL) << 16) | ((tokenData.byte(1) & 0xFFL) << 8) | (tokenData.byte(0) & 0xFFL)
          ESExprToken.Float64Value(java.lang.Double.longBitsToDouble(floatAsLong))
      }

      (Some(WithLocation(token, Location(fileName, startOffset, endOffset))), ReadState.Empty(endOffset), remaining)
    end if
  end readBuffer

  def tokenize(fileName: Option[String]): ZChannel[Any, Nothing, Chunk[Byte], Any, ESExprError, Chunk[WithLocation[ESExprToken, FileOffset]], FileOffset] =
    ZChannelUtil.mapAccumChunksZIO[Any, ESExprError, Chunk[Byte], Chunk[WithLocation[ESExprToken, FileOffset]], ReadState](ReadState.Empty(FileOffset(0))) { (state, data) =>
      readTokens(state, fileName, data, prevTokens = Chunk.empty)
    }
      .flatMap {
        case ReadState.Empty(offset) => ZChannel.succeed(offset)
        case _ => ZChannel.fail(EOFException())
      }

}
