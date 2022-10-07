package dev.argon.argonvm.engine;

import dev.argon.argonvm.*;
import org.jetbrains.annotations.Nullable;

import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

public class StandardLibrary implements NativeFunctions {
	public void print(String value) throws Throwable {
		System.out.println(value);
	}

	public String int32_to_string(int value) throws Throwable {
		return Integer.toString(value);
	}

	public String uint32_to_string(int value) throws Throwable {
		return Integer.toUnsignedString(value);
	}

	public String float64_to_string(double value) throws Throwable {
		return Double.toString(value);
	}

	WasmMemory mainMemory = null;

	public WasmMemory wasm_memory_new(int pages) throws Throwable {
		WasmMemory memory = new WasmMemory(pages);
		if(mainMemory == null) {
			synchronized(this) {
				if(mainMemory == null) {
					mainMemory = memory;
				}
			}
		}
		return memory;
	}

	public void wasm_memory_store_i32(int addr, int value, byte align, int offset, WasmMemory memory) throws Throwable {
		memory.storeI32(addr, value, align, offset);
	}

	public void wasm_memory_store_buffer(WasmMemory memory, int addr, byte[] data) throws Throwable {
		memory.storeBuffer(addr, data);
	}

	public int wasi_fd_write(int fd, int iovs, int iovs_len, int nwritten) throws Throwable {
		OutputStream os;
		switch(fd) {
			case 1:
				os = System.out;
				break;

			default:
				throw new Exception("Unknown fd");
		}

		int bytesWritten = 0;
		for(int i = 0; i < iovs_len; ++i) {
			int iov_base = mainMemory.loadI32(iovs, (byte)0, i * 8);
			int iov_len = mainMemory.loadI32(iovs, (byte)0, i * 8 + 4);
			byte[] data = mainMemory.loadBuffer(iov_base, iov_len);
			os.write(data);
			bytesWritten += iov_len;
		}
		mainMemory.storeI32(nwritten, bytesWritten, (byte)0, 0);
		return bytesWritten;
	}

	@Override
	public final @Nullable VMFunction get(String name) {
		return switch(name) {
			case "print" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.ObjectReference());
				}

				@Override
				public VMType returnType() {
					return new VMType.Tuple(List.of());
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (String)args[0];
					print(value);
					return new NativeTrampoline.Result(new VMTuple(new Object[] {}));
				}
			};

			case "int32_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Int32());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (int)args[0];
					String result = int32_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			case "uint32_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Int32());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (int)args[0];
					String result = uint32_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			case "float64_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Float64());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (double)args[0];
					String result = float64_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			case "wasm_memory_new" -> new NativeVMFunction() {
				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					int pages = (int)args[0];
					WasmMemory memory = wasm_memory_new(pages);
					return new NativeTrampoline.Result(memory);
				}

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Int32());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}
			};

			case "wasm_memory_store_i32" -> new NativeVMFunction() {
				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					int addr = (int)args[0];
					int value = (int)args[1];
					byte align = (byte)args[2];
					int offset = (int)args[3];
					WasmMemory memory = (WasmMemory)args[4];
					wasm_memory_store_i32(addr, value, align, offset, memory);
					return new NativeTrampoline.Result(new VMTuple(new Object[] {}));
				}

				@Override
				public List<VMType> parameterTypes() {
					return List.of(
						new VMType.Int32(),
						new VMType.Int32(),
						new VMType.Int8(),
						new VMType.Int32(),
						new VMType.ObjectReference()
					);
				}

				@Override
				public VMType returnType() {
					return new VMType.Tuple(List.of());
				}
			};

			case "wasm_memory_store_buffer" -> new NativeVMFunction() {
				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					WasmMemory memory = (WasmMemory)args[0];
					int addr = (int)args[1];
					byte[] data = (byte[])args[2];
					wasm_memory_store_buffer(memory, addr, data);
					return new NativeTrampoline.Result(new VMTuple(new Object[] {}));
				}

				@Override
				public List<VMType> parameterTypes() {
					return List.of(
						new VMType.ObjectReference(),
						new VMType.Int32(),
						new VMType.ObjectReference()
					);
				}

				@Override
				public VMType returnType() {
					return new VMType.Tuple(List.of());
				}
			};

			case "wasi_snapshot_preview1::fd_write" -> new NativeVMFunction() {
				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					int fd = (int)args[0];
					int iovs = (int)args[1];
					int iovs_len = (int)args[2];
					int nwritten = (int)args[3];
					int bytes_written = wasi_fd_write(fd, iovs, iovs_len, nwritten);
					return new NativeTrampoline.Result(bytes_written);
				}

				@Override
				public List<VMType> parameterTypes() {
					return List.of(
						new VMType.Int32(),
						new VMType.Int32(),
						new VMType.Int32(),
						new VMType.Int32()
					);
				}

				@Override
				public VMType returnType() {
					return new VMType.Int32();
				}
			};

			default -> null;
		};
	}
}
