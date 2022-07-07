package dev.argon.vm;

import java.util.List;
import dev.argon.vm.format.VMType;
import org.objectweb.asm.Type;
import org.objectweb.asm.Opcodes;

final class TypeUtils {
    
    public static Type getType(VMType.V1 t) {
        return switch(t) {
            case VMType.V1.I8 u -> Type.BYTE_TYPE;
            case VMType.V1.I16 u -> Type.SHORT_TYPE;
            case VMType.V1.I32 u -> Type.INT_TYPE;
            case VMType.V1.I64 u -> Type.LONG_TYPE;
            case VMType.V1.F32 u -> Type.FLOAT_TYPE;
            case VMType.V1.F64 u -> Type.DOUBLE_TYPE;
            case VMType.V1.Gcref u -> Type.getObjectType("dev/argon/vm/GCRef");
            case VMType.V1.Gcptr u -> Type.getObjectType("java/lang/Object");
            case VMType.V1.Tuple tuple -> {
                var elements = tuple.tuple();
                var sb = new StringBuilder();
                sb.append("dev/argon/vm/tuple/Tuple");

                for(int i = 0; i < elements.size(); ++i) {
                    sb.append(getTupleElementTypeStr(elements.get(i)));
                }

                yield Type.getObjectType(sb.toString());
            }
        };
    }

    private static String getTupleElementTypeStr(VMType.V1 t) {
        return switch(t) {
            case VMType.V1.I8 u -> "B";
            case VMType.V1.I16 u -> "S";
            case VMType.V1.I32 u -> "I";
            case VMType.V1.I64 u -> "J";
            case VMType.V1.F32 u -> "F";
            case VMType.V1.F64 u -> "D";
            case VMType.V1.Gcref u -> "O";
            case VMType.V1.Gcptr u -> "O";
            case VMType.V1.Tuple u -> "O";
        };
    }

    public static void getStackType(VMType.V1 t, List<Object> types) {
        switch(t) {
            case VMType.V1.I8 u -> types.add(Opcodes.INTEGER);
            case VMType.V1.I16 u -> types.add(Opcodes.INTEGER);
            case VMType.V1.I32 u -> types.add(Opcodes.INTEGER);
            case VMType.V1.I64 u -> {
                types.add(Opcodes.LONG);
                types.add(Opcodes.TOP);
            }
            case VMType.V1.F32 u -> types.add(Opcodes.FLOAT);
            case VMType.V1.F64 u -> {
                types.add(Opcodes.DOUBLE);
                types.add(Opcodes.TOP);
            }
            case VMType.V1.Gcref u -> types.add("dev/argon/vm/GCRef");
            case VMType.V1.Gcptr u -> types.add("java/lang/Object");
            case VMType.V1.Tuple tuple -> {
                var elements = tuple.tuple();
                var sb = new StringBuilder();
                sb.append("dev/argon/vm/tuple/Tuple");

                for(int i = 0; i < elements.size(); ++i) {
                    sb.append(getTupleElementTypeStr(elements.get(i)));
                }

                types.add(sb.toString());
            }
        };
    }

}
