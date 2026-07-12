package argontube2.Argon.Core.U32;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class U32Externs {
    private U32Externs() {}

    @ExternFunction("u32_to_s")
    public static Trampoline<String> u32ToS(int i, Tuple0 _empty) {
        return new Trampoline.Result<>(Integer.toUnsignedString(i));
    }
}
