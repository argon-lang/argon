package argontube2.Argon.IO;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Proxy;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;

import dev.argon.runtime.ExternFunction;
import dev.argon.runtime.Trampoline;
import dev.argon.runtime.Tuple0;
import dev.argon.runtime.TypeInfo;

public final class PathExterns {
    private PathExterns() {}

    @ExternFunction("path_from_string")
    public static Trampoline<Path> fromString(String path) {
        return new Trampoline.Result<>(new NativePath(path));
    }

    private static final class NativePath implements Path {
        private NativePath(String path) {
            this.path = path;
        }

        private final String path;

        @Override
        public Trampoline<String> display$a$r$bstring$a$e() {
            return new Trampoline.Result<>(path);
        }

        @Override
        public Trampoline<Object> open_read$a$r$rResource$a$_$r$_$a$rInputStream$a$r$_$a$e$e() {
            try {
                var stream = new NativeInputStream(Files.newInputStream(Paths.get(path)));
                return new Trampoline.Result<>(resource(stream, InputStream.class, stream::close));
            }
            catch(IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }

        @Override
        public Trampoline<Object> open_write$a$r$rResource$a$_$r$_$a$rOutputStream$a$r$_$a$e$e() {
            try {
                var stream = new NativeOutputStream(Files.newOutputStream(Paths.get(path)));
                return new Trampoline.Result<>(resource(stream, OutputStream.class, stream::close));
            }
            catch(IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }
    }

    private static final class NativeInputStream implements InputStream {
        private NativeInputStream(java.io.InputStream stream) {
            this.stream = stream;
        }

        private final java.io.InputStream stream;

        @Override
        public Trampoline<BigInteger> read$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$bint$a$e(
            byte[] array,
            BigInteger offset,
            BigInteger count
        ) {
            try {
                int result = stream.read(array, offset.intValueExact(), count.intValueExact());
                return new Trampoline.Result<>(BigInteger.valueOf(Math.max(result, 0)));
            }
            catch(IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }

        private void close() throws IOException {
            stream.close();
        }
    }

    private static final class NativeOutputStream implements OutputStream {
        private NativeOutputStream(java.io.OutputStream stream) {
            this.stream = stream;
        }

        private final java.io.OutputStream stream;

        @Override
        public Trampoline<Tuple0> write$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$t$e(
            byte[] array,
            BigInteger offset,
            BigInteger count
        ) {
            try {
                stream.write(array, offset.intValueExact(), count.intValueExact());
                return new Trampoline.Result<>(new Tuple0());
            }
            catch(IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }

        private void close() throws IOException {
            stream.close();
        }
    }

    private static Object resource(Object stream, Class<?> streamType, CloseOperation close) {
        try {
            var resourceType = Class.forName("argontube2.Argon.IO.Resource");
            return Proxy.newProxyInstance(
                PathExterns.class.getClassLoader(),
                new Class<?>[] { resourceType },
                (_proxy, method, _args) -> switch(method.getName()) {
                    case ":pt0" -> new TypeInfo(streamType);
                    case "resource$a$r$_" -> new Trampoline.Result<>(stream);
                    case "close$a$t$e$r$t$e" -> {
                        try {
                            close.close();
                            yield new Trampoline.Result<>(new Tuple0());
                        }
                        catch(IOException ex) {
                            throw new UncheckedIOException(ex);
                        }
                    }
                    default -> throw new UnsupportedOperationException(method.getName());
                }
            );
        }
        catch(ClassNotFoundException ex) {
            throw new IllegalStateException("Argon.IO Resource interface is missing", ex);
        }
    }

    @FunctionalInterface
    private interface CloseOperation {
        void close() throws IOException;
    }
}
