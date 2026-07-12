package argontube2.Argon.Core.I16;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class I16Externs {
    private I16Externs() {}

    @ExternFunction("i16_to_s")
    public static Trampoline<String> i16ToS(short s, Tuple0 _empty) {
        return new Trampoline.Result<>(Short.toString(s));
    }
}
