package argontube2.Argon.Core.Puts;

import java.math.BigInteger;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

class PutsExterns {
    private PutsExterns() {}

    @ExternFunction("puts")
    public static Trampoline<Tuple0> puts(String s) {
        System.out.println(s);
        return new Trampoline.Result<>(new Tuple0());
    }
}

