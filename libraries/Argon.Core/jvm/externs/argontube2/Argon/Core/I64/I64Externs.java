package argontube2.Argon.Core.I64;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class I64Externs {
    private I64Externs() {}

    @ExternFunction("i64_to_s")
    public static Trampoline<String> i64ToS(long i, Tuple0 _empty) {
        return new Trampoline.Result<>(Long.toString(i));
    }
}
