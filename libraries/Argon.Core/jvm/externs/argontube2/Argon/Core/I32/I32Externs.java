package argontube2.Argon.Core.I32;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class I32Externs {
    private I32Externs() {}

    @ExternFunction("i32_to_s")
    public static Trampoline<String> i32ToS(int i, Tuple0 _empty) {
        return new Trampoline.Result<>(Integer.toString(i));
    }
}
