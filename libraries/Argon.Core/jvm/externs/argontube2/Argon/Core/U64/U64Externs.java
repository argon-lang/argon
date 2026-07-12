package argontube2.Argon.Core.U64;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class U64Externs {
    private U64Externs() {}

    @ExternFunction("u64_to_s")
    public static Trampoline<String> u64ToS(long i, Tuple0 _empty) {
        return new Trampoline.Result<>(Long.toUnsignedString(i));
    }
}
