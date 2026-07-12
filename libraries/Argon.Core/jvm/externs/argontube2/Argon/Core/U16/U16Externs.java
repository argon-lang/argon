package argontube2.Argon.Core.U16;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class U16Externs {
    private U16Externs() {}

    @ExternFunction("u16_to_s")
    public static Trampoline<String> u16ToS(short s, Tuple0 _empty) {
        return new Trampoline.Result<>(Integer.toString(Short.toUnsignedInt(s)));
    }
}
