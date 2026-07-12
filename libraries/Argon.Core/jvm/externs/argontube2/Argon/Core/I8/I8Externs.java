package argontube2.Argon.Core.I8;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;


class I8Externs {
    private I8Externs() {}

    @ExternFunction("i8_to_s")
    public static Trampoline<String> i8ToS(byte b, Tuple0 _empty) {
        return new Trampoline.Result<>(Byte.toString(b));
    }
}
