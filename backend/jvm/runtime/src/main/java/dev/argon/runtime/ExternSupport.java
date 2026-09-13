package dev.argon.runtime;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Proxy;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Runtime implementations used by generated standard-library extern entry
 * points.
 */
public final class ExternSupport {
	private ExternSupport() {
	}

	public static Trampoline<String> stringResult(String value) {
		return new Trampoline.Result<>(value);
	}

	public static Trampoline<String> i8ToS(byte value, Tuple0 _empty) {
		return new Trampoline.Result<>(Byte.toString(value));
	}
	public static Trampoline<String> i16ToS(short value, Tuple0 _empty) {
		return new Trampoline.Result<>(Short.toString(value));
	}
	public static Trampoline<String> i32ToS(int value, Tuple0 _empty) {
		return new Trampoline.Result<>(Integer.toString(value));
	}
	public static Trampoline<String> i64ToS(long value, Tuple0 _empty) {
		return new Trampoline.Result<>(Long.toString(value));
	}
	public static Trampoline<String> u8ToS(byte value, Tuple0 _empty) {
		return new Trampoline.Result<>(Integer.toString(Byte.toUnsignedInt(value)));
	}
	public static Trampoline<String> u16ToS(short value, Tuple0 _empty) {
		return new Trampoline.Result<>(Integer.toString(Short.toUnsignedInt(value)));
	}
	public static Trampoline<String> u32ToS(int value, Tuple0 _empty) {
		return new Trampoline.Result<>(Integer.toUnsignedString(value));
	}
	public static Trampoline<String> u64ToS(long value, Tuple0 _empty) {
		return new Trampoline.Result<>(Long.toUnsignedString(value));
	}
	public static Trampoline<String> intToS(BigInteger value, Tuple0 _empty) {
		return new Trampoline.Result<>(value.toString());
	}
	public static Trampoline<Boolean> stringStartsWith(String value, String prefix) {
		return new Trampoline.Result<>(value.startsWith(prefix));
	}
	public static Trampoline<Boolean> stringEndsWith(String value, String suffix) {
		return new Trampoline.Result<>(value.endsWith(suffix));
	}
	public static Trampoline<Boolean> stringContains(String value, String substring) {
		return new Trampoline.Result<>(value.contains(substring));
	}
	public static Trampoline<Tuple0> puts(String value) {
		System.out.println(value);
		return new Trampoline.Result<>(new Tuple0());
	}

	public static Trampoline<Object> pathFromString(String path) {
		try {
			Class<?> pathType = Class.forName("argontube2.Argon.IO.Path");
			Object value = Proxy.newProxyInstance(ExternSupport.class.getClassLoader(), new Class<?>[]{pathType},
				(_proxy, method, _args) -> switch (method.getName()) {
					case "display:a:r:bstring:a:e" -> new Trampoline.Result<>(path);
					case "open_read:a:r:rResource:a:_:r:_:a:rInputStream:a:r:_:a:e:e" -> openRead(path);
					case "open_write:a:r:rResource:a:_:r:_:a:rOutputStream:a:r:_:a:e:e" -> openWrite(path);
					default -> throw new UnsupportedOperationException(method.getName());
				});
			return new Trampoline.Result<>(value);
		}
		catch(ClassNotFoundException ex) {
			throw new IllegalStateException("Argon.IO Path interface is missing", ex);
		}
	}

	private static Trampoline<Object> openRead(String path) {
		try {
			var stream = Files.newInputStream(Paths.get(path));
			Class<?> type = Class.forName("argontube2.Argon.IO.InputStream");
			Object value = Proxy.newProxyInstance(ExternSupport.class.getClassLoader(), new Class<?>[]{type},
				(_proxy, method, args) -> {
					if(!method.getName().equals("read:a:barray:a:bu8:a:e:e:bint:a:e:bint:a:e:_:r:bint:a:e")) {
						throw new UnsupportedOperationException(method.getName());
					}
					var nonNullArgs = java.util.Objects.requireNonNull(args);
					try {
						int result = stream.read((byte[]) nonNullArgs[0], ((BigInteger) nonNullArgs[1]).intValueExact(),
							((BigInteger) nonNullArgs[2]).intValueExact());
						return new Trampoline.Result<>(BigInteger.valueOf(Math.max(result, 0)));
					}
					catch(IOException ex) {
						throw new UncheckedIOException(ex);
					}
				});
			return new Trampoline.Result<>(resource(value, type, stream::close));
		}
		catch(IOException ex) {
			throw new UncheckedIOException(ex);
		}
		catch(ClassNotFoundException ex) {
			throw new IllegalStateException("Argon.IO InputStream interface is missing", ex);
		}
	}

	private static Trampoline<Object> openWrite(String path) {
		try {
			var stream = Files.newOutputStream(Paths.get(path));
			Class<?> type = Class.forName("argontube2.Argon.IO.OutputStream");
			Object value = Proxy.newProxyInstance(ExternSupport.class.getClassLoader(), new Class<?>[]{type},
				(_proxy, method, args) -> {
					if(!method.getName().equals("write:a:barray:a:bu8:a:e:e:bint:a:e:bint:a:e:_:r:t:e")) {
						throw new UnsupportedOperationException(method.getName());
					}
					var nonNullArgs = java.util.Objects.requireNonNull(args);
					try {
						stream.write((byte[]) nonNullArgs[0], ((BigInteger) nonNullArgs[1]).intValueExact(),
							((BigInteger) nonNullArgs[2]).intValueExact());
						return new Trampoline.Result<>(new Tuple0());
					}
					catch(IOException ex) {
						throw new UncheckedIOException(ex);
					}
				});
			return new Trampoline.Result<>(resource(value, type, stream::close));
		}
		catch(IOException ex) {
			throw new UncheckedIOException(ex);
		}
		catch(ClassNotFoundException ex) {
			throw new IllegalStateException("Argon.IO OutputStream interface is missing", ex);
		}
	}

	private static Object resource(Object stream, Class<?> streamType, CloseOperation close) {
		try {
			Class<?> resourceType = Class.forName("argontube2.Argon.IO.Resource");
			return Proxy.newProxyInstance(ExternSupport.class.getClassLoader(), new Class<?>[]{resourceType},
				(_proxy, method, _args) -> switch (method.getName()) {
					case ":pt0" -> new TypeInfo(streamType);
					case "resource:a:r:_" -> new Trampoline.Result<>(stream);
					case "close:a:t:e:r:t:e" -> {
						try {
							close.close();
							yield new Trampoline.Result<>(new Tuple0());
						}
						catch(IOException ex) {
							throw new UncheckedIOException(ex);
						}
					}
					default -> throw new UnsupportedOperationException(method.getName());
				});
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
