package dev.argon.plugin.adapter.js;

import com.google.protobuf.*;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyObject;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class ProtoConverter {
    private ProtoConverter() {}

    // Assumes already locked
    public static Value messageToJSProto(JSEnv<?> env, GeneratedMessageV3 message) {
        Map<String, Object> values = new HashMap<>();

        for(var field : message.getAllFields().entrySet()) {
            var fieldDesc = field.getKey();
            var fieldValue = field.getValue();

            Value convValue;
            if(fieldDesc.isRepeated()) {
                var items = (List<?>)fieldValue;
                var arr = new Value[items.size()];
                for(int i = 0; i < items.size(); ++i) {
                    arr[i] = fieldValueToJSProto(env, fieldDesc, items.get(i));
                }
                convValue = env.context.asValue(arr);
            }
            else {
                convValue = fieldValueToJSProto(env, fieldDesc, fieldValue);
            }

            values.put(fieldDesc.getName(), convValue);
        }

        for(var field : message.getDescriptorForType().getFields()) {
            if(values.containsKey(field.getName())) {
                continue;
            }

            if(field.isRepeated()) {
                values.put(field.getName(), new Value[] {});
            }
            else {
                values.put(field.getName(), env.context.eval("js", "undefined"));
            }
        }

        return env.context.asValue(ProxyObject.fromMap(values));
    }

    private static Value fieldValueToJSProto(JSEnv<?> env, Descriptors.FieldDescriptor fieldDesc, Object fieldValue) {
        return switch(fieldDesc.getType()) {
            case DOUBLE, FLOAT, INT32, SFIXED32, SINT32, BOOL, STRING -> env.context.asValue(fieldValue);
            case INT64, SFIXED64, SINT64 -> env.context.eval("js", "BigInt").execute(fieldValue.toString());
            case UINT64, FIXED64 -> env.context.eval("js", "BigInt").execute(Long.toUnsignedString((long)fieldValue));
            case UINT32, FIXED32 -> env.context.asValue(Integer.toUnsignedLong((int)fieldValue));
            case MESSAGE -> messageToJSProto(env, (GeneratedMessageV3)fieldValue);
            case ENUM -> env.context.asValue(((ProtocolMessageEnum)fieldValue).getValueDescriptor().getName());

            case BYTES -> {
                var bytes = (ByteString)fieldValue;
                var arr = env.context.eval("js", "n => new Uint8Array(n)").execute(bytes.size());
                for(int i = 0; i < bytes.size(); ++i) {
                    arr.setArrayElement(i, bytes.byteAt(i));
                }
                yield arr;
            }

            case GROUP -> throw new RuntimeException("Unexpected field type");
        };
    }

    public static <BuilderT extends Message.Builder> void buildFromJSProto(JSEnv<?> env, BuilderT builder, Value jsValue) {
        for(var fieldDesc : builder.getDescriptorForType().getFields()) {
            if(!jsValue.hasMember(fieldDesc.getName())) {
                continue;
            }

            var fieldValue = jsValue.getMember(fieldDesc.getName());
            if(fieldDesc.isRepeated()) {
                for(long i = 0; i < fieldValue.getArraySize(); ++i) {
                    builder.addRepeatedField(fieldDesc, fieldValueFromJSProto(env, builder, fieldDesc, fieldValue.getArrayElement(i)));
                }
            }
            else {
                builder.setField(fieldDesc, fieldValueFromJSProto(env, builder, fieldDesc, fieldValue));
            }
        }
    }

    private static <BuilderT extends Message.Builder> Object fieldValueFromJSProto(JSEnv<?> env, BuilderT parentBuilder, Descriptors.FieldDescriptor fieldDesc, Value jsValue) {
        return switch(fieldDesc.getType()) {
            case DOUBLE -> jsValue.asDouble();
            case FLOAT -> jsValue.asFloat();
            case INT32, SFIXED32, SINT32 -> jsValue.asInt();
            case BOOL -> jsValue.asBoolean();
            case STRING -> jsValue.asString();
            case INT64, SFIXED64, SINT64, UINT64, FIXED64 -> jsValue.as(BigInteger.class).longValue();
            case UINT32, FIXED32 -> (int)jsValue.asLong();
            case MESSAGE -> {
                var builder = parentBuilder.newBuilderForField(fieldDesc);
                buildFromJSProto(env, builder, jsValue);
                yield builder;
            }
            case ENUM -> fieldDesc.getEnumType().findValueByName(jsValue.asString());

            case BYTES -> {
                var os = ByteString.newOutput((int)jsValue.getArraySize());
                for(long i = 0; i < jsValue.getArraySize(); ++i) {
                    os.write((byte)jsValue.getArrayElement(i).asShort());
                }
                yield os.toByteString();
            }

            case GROUP -> throw new RuntimeException("Unexpected field type");
        };
    }

}
