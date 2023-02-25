package dev.argon.plugin.loader.js

import com.google.protobuf.ByteString
import com.google.protobuf.descriptor.FieldDescriptorProto
import dev.argon.tube.GeneratedEnumInstances.given
import dev.argon.util.{*, given}
import org.graalvm.polyglot.proxy.ProxyObject
import org.graalvm.polyglot.{Context, Value}
import scalapb.descriptors.*
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import scala.collection.mutable

object ProtoConverterJS {
  def convertPValueToJS(value: PValue, unsigned: Boolean = false)(using context: Context): Value =
    value.asInstanceOf[PValue & Matchable] match {
      case PInt(value) if unsigned =>
        context.asValue(Integer.toUnsignedLong(value).toDouble).nn

      case PLong(value) if unsigned =>
        context.eval("js", "BigInt").nn.execute(java.lang.Long.toUnsignedString(value)).nn

      case PInt(value) => context.asValue(value).nn
      case PLong(value) =>
        context.eval("js", "BigInt").nn.execute(value.toString).nn

      case PString(value) => context.asValue(value).nn
      case PDouble(value) => context.asValue(value).nn
      case PFloat(value) => context.asValue(value).nn
      case PBoolean(value) => context.asValue(value).nn
      case PByteString(value) =>
        ValueEncoder[Uint8Array].encode(TypedArrayUtil.fromByteArray(value.toByteArray().nn))

      case PEnum(value) => context.asValue(value.number).nn

      case PMessage(objValue) =>
        val obj = java.util.HashMap[String, Any]()
        for (field, fieldValue) <- objValue do
          val fieldUnsigned = field.protoType match {
            case FieldDescriptorProto.Type.TYPE_UINT32 |
              FieldDescriptorProto.Type.TYPE_FIXED32 |
                 FieldDescriptorProto.Type.TYPE_UINT64 |
                 FieldDescriptorProto.Type.TYPE_FIXED64 =>
              true

            case _ => false
          }

          field.containingOneof match
            case Some(oneof) =>
              val caseObj = java.util.HashMap[String, Any]()
              caseObj.put("$case", field.name)
              caseObj.put(field.name, convertPValueToJS(fieldValue, fieldUnsigned))
              obj.put(oneof.name, ProxyObject.fromMap(caseObj))

            case None =>
              obj.put(field.name, convertPValueToJS(fieldValue, false))
          end match
        end for
        context.asValue(ProxyObject.fromMap(obj)).nn

      case PRepeated(items) =>
        val arr = context.eval("js", "n => new Array(n)").nn.execute(items.size).nn
        for (item, i) <- items.zipWithIndex do
          arr.setArrayElement(i, convertPValueToJS(items(i), unsigned))

        arr

      case _: PEmpty.type => context.eval("js", "undefined").nn
    }

  def convertJSToPMessage(value: Value, descriptor: Descriptor)(using Context): PMessage =
    val objValue = mutable.Map[FieldDescriptor, PValue]()
    for field <- descriptor.fields do
      field.containingOneof match
        case Some(oneof) =>
          if value.hasMember(oneof.name) then
            val oneofValue = value.getMember(oneof.name).nn
            if oneofValue.getMember("$case").nn.asString() == oneof.name then
              objValue(field) = convertJSToPValue(oneofValue.getMember(field.name).nn, field)
            end if
          end if
        case None =>
          if field.isRepeated then
            val arr = value.getMember(field.name).nn
            objValue(field) = PRepeated((0 until arr.getArraySize.toInt).map { i =>  convertJSToPValue(arr.getArrayElement(i).nn, field) }.toVector)
          else if value.hasMember(field.name) then
            objValue(field) = convertJSToPValue(value.getMember(field.name).nn, field)
          end if
      end match
    end for

    PMessage(objValue.toMap)

  private def convertJSToPValue(value: Value, fieldDescriptor: FieldDescriptor)(using context: Context): PValue =
    if value.isNull then
      PEmpty
    else
      fieldDescriptor.protoType match
        case FieldDescriptorProto.Type.TYPE_BOOL => PBoolean(value.asBoolean())

        case FieldDescriptorProto.Type.TYPE_INT32 | FieldDescriptorProto.Type.TYPE_SINT32 | FieldDescriptorProto.Type.TYPE_SFIXED32 =>
          PInt(value.asInt())

        case FieldDescriptorProto.Type.TYPE_UINT32 | FieldDescriptorProto.Type.TYPE_FIXED32 =>
          PInt(value.asLong().toInt)

        case FieldDescriptorProto.Type.TYPE_INT64 | FieldDescriptorProto.Type.TYPE_SINT64 | FieldDescriptorProto.Type.TYPE_SFIXED64 =>
          PLong(value.asLong())
        case FieldDescriptorProto.Type.TYPE_UINT64 | FieldDescriptorProto.Type.TYPE_FIXED64 =>
          PLong(java.lang.Long.parseUnsignedLong(context.eval("js", "n => n.toString()").nn.asString()))

        case FieldDescriptorProto.Type.TYPE_DOUBLE => PDouble(value.asDouble())
        case FieldDescriptorProto.Type.TYPE_FLOAT => PFloat(value.asFloat())

        case FieldDescriptorProto.Type.TYPE_STRING => PString(value.asString().nn)

        case FieldDescriptorProto.Type.TYPE_BYTES =>
          val ta = ValueDecoder[Uint8Array].decode(value)
          PByteString(ByteString.copyFrom(TypedArrayUtil.toByteArray(ta)).nn)

        case FieldDescriptorProto.Type.TYPE_MESSAGE =>
          val desc = fieldDescriptor.scalaType.asInstanceOf[ScalaType.Message].descriptor
          convertJSToPMessage(value, desc)

        case FieldDescriptorProto.Type.TYPE_ENUM =>
          val desc = fieldDescriptor.scalaType.asInstanceOf[ScalaType.Enum].descriptor
          PEnum(desc.findValueByNumber(value.asInt()).get)

        case FieldDescriptorProto.Type.TYPE_GROUP | FieldDescriptorProto.Type.Unrecognized(_) => throw new RuntimeException("Unexpected type")
      end match


}

