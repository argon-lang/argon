package argontube2.Argon.Core.U8;


import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class U8Externs {
    private U8Externs() {}

    @ExternFunction("u8_to_s")
    public static Trampoline<String> u8ToS(byte b, Tuple0 _empty) {
        return new Trampoline.Result<>(Integer.toString(Byte.toUnsignedInt(b)));
    }
}

