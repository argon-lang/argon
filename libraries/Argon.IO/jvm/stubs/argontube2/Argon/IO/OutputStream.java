package argontube2.Argon.IO;

import java.math.BigInteger;

import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;

public interface OutputStream {
    Trampoline<Tuple0> write$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$t$e(
        byte[] array,
        BigInteger offset,
        BigInteger count
    );
}
