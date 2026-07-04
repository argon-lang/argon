package argontube2.Argon.Core.Int;

import java.math.BigInteger;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;

class IntExterns {
    private IntExterns() {}

    @ExternFunction("int_to_s")
    public static Trampoline<String> intToS(BigInteger i) {
        return new Trampoline.Result<>(i.toString());
    }
}

