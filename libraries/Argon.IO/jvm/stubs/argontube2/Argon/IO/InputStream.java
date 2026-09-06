package argontube2.Argon.IO;

import java.math.BigInteger;

import dev.argon.runtime.Trampoline;

public interface InputStream {
    Trampoline<BigInteger> read$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$bint$a$e(
        byte[] array,
        BigInteger offset,
        BigInteger count
    );
}
