package argontube2.Argon.Core.Int;

import java.math.BigInteger;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class IntExterns {
    private IntExterns() {}

    @ExternFunction("int_to_s")
    public static Trampoline<String> intToS(BigInteger i, Tuple0 _empty) {
        return new Trampoline.Result<>(i.toString());
    }
}

