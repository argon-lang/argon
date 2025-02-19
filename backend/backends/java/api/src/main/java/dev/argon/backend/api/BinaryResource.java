package dev.argon.backend.api;

import dev.argon.nobleidl.runtime.ErrorType;
import dev.argon.nobleidl.runtime.graaljsInterop.*;
import dev.argon.nobleidl.runtime.util.IOErrorType;
import dev.argon.nobleidl.runtime.util.InputStreamWithError;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.util.Objects;
import java.util.Optional;

public abstract class BinaryResource<TE> {
	public BinaryResource() {}

	public abstract Optional<String> fileName();

	public abstract <EE extends IOException> InputStreamWithError<EE> asInputStream(IOErrorType<TE, EE> errorType) throws InterruptedIOException, EE;
	
	

	public static <TE> JSAdapter<BinaryResource<TE>> jsAdapter(JSAdapter<TE> eAdapter, ErrorType<TE, ?> errorTypeThrowable_e) {
		return new JSAdapterImpl<>(eAdapter, IOErrorType.fromErrorType(errorTypeThrowable_e));
	}
	
	private static class JSAdapterImpl<TE, IOE extends IOException> implements JSAdapter<BinaryResource<TE>> {
		public JSAdapterImpl(JSAdapter<TE> eAdapter, IOErrorType<TE, IOE> errorTypeIO) {
			this.eAdapter = eAdapter;
			this.errorTypeIO = errorTypeIO;
		}

		private final JSAdapter<TE> eAdapter;
		private final IOErrorType<TE, IOE> errorTypeIO;

		@Override
		public BinaryResource<TE> fromJS(Context context, JSExecutor jsExecutor, Value value) {
			Optional<String> fileNameOpt;
			{
				var fileName = value.getMember("fileName");
				if(fileName == null || fileName.isNull()) {
					fileNameOpt = Optional.<String>empty();
				}
				else {
					fileNameOpt = Optional.of(fileName.asString());
				}
			}
			
			return new BinaryResource<TE>() {
				@Override
				public Optional<String> fileName() {
					return fileNameOpt;
				}

				@Override
				public <EE extends IOException> InputStreamWithError<EE> asInputStream(IOErrorType<TE, EE> errorType) throws InterruptedIOException, EE {
					try {
						return jsExecutor.runOnJSThreadWithError(() -> {
							var iter = context.eval("js", "res => res.asBytes()[Symbol.asyncIterable]()").execute(value);
							return new InputStreamWithError<EE>() {
								private boolean isDone = false;
								private Value data = null;
								private long dataOffset;
	
								@Override
								public int read() throws InterruptedIOException, EE {
									byte[] b = new byte[1];
									int bytesRead;
									do {
										bytesRead = read(b);
										if(bytesRead < 0) {
											return -1;
										}
									} while(read(b) < 1);
									return Byte.toUnsignedInt(b[0]);
								}
	
								@Override
								public int read(byte @NotNull[] b, int off, int len) throws InterruptedIOException, EE {
									Objects.checkFromIndexSize(off, len, b.length);
									try {
										if(!isDone && data == null) {
											Value iterRes = CallUtil.callJSFunction(context, jsExecutor, JSAdapter.VALUE_ADAPTER, eAdapter, errorType, ErrorTypeAdapter.toJS(context, jsExecutor, errorType), () -> iter.invokeMember("next"));
	
											dataOffset = 0;
											data = jsExecutor.runOnJSThreadWithError(() -> {
												if(iterRes.getMember("done").asBoolean()) {
													isDone = true;
													return null;
												}
												else {
													return iterRes.getMember("value");
												}
											}).get();
										}
	
										if(data == null) {
											return -1;
										}
	
										return jsExecutor.runOnJSThreadWithoutError(() -> {
											int readLen = len;
											long arrSize = data.getArraySize();
											long remaining = arrSize - dataOffset;
											boolean fullRead = false;
	
											if(remaining < readLen) {
												readLen = (int)remaining;
											}
											else {
												fullRead = true;
											}
	
											for(int i = 0; i < readLen; ++i) {
												b[off + i] = (byte)data.getArrayElement(dataOffset + i).asInt();
											}
	
											if(fullRead) {
												data = null;
											}
	
											return readLen;
										}).get();
									}
									catch(InterruptedException e) {
										var ex = new InterruptedIOException(e.getMessage());
										ex.setStackTrace(e.getStackTrace());
										throw ex;
									}
								}
							};
						}).get();
					}
					catch(InterruptedException e) {
						var ex = new InterruptedIOException(e.getMessage());
						ex.setStackTrace(e.getStackTrace());
						throw ex;
					}
				}
			};
		}

		@Override
		public Value toJS(Context context, JSExecutor jsExecutor, BinaryResource<TE> resource) {
			var openStreamFunc = CallUtil.makeJavaScriptFunction(_ -> {
				return new JSPromiseWithErrorAdapter<>(JSAdapter.identity(), eAdapter).toJS(context, jsExecutor, errorTypeIO, jsExecutor.offloadJavaWithError(() -> {
					return errorTypeIO.<InputStreamWithError<IOE>, IOE>catchingIO(
						() -> resource.asInputStream(errorTypeIO),
						error -> {
							throw context.eval("js", "ex => { throw ex; }").execute(error).throwException();
						}
					);
				}));
			});

			var streamPull = CallUtil.makeJavaScriptFunction(arguments -> {
				return new JSPromiseWithErrorAdapter<>(JSAdapter.VALUE_ADAPTER, eAdapter).toJS(context, jsExecutor, errorTypeIO, jsExecutor.offloadJavaWithError(() -> {
					var is = JSAdapter.<InputStream>identity().fromJS(context, jsExecutor, arguments[0]);

					byte[] buff = new byte[4096];
					int bytesRead = is.read(buff);
					if(bytesRead < 0) {
						return null;
					}

					var arr = context.eval("js", "n => new Uint8Array(n)").execute(bytesRead);
					for(int i = 0; i < bytesRead; ++i) {
						arr.setArrayElement(i, Byte.toUnsignedInt(buff[i]));
					}
					return arr;
				}));
			});

			var streamClose = CallUtil.makeJavaScriptFunction(arguments -> {
				return new JSPromiseWithErrorAdapter<>(JSAdapter.identity(), eAdapter).toJS(context, jsExecutor, errorTypeIO, jsExecutor.offloadJavaWithError(() -> {
					var is = JSAdapter.<InputStream>identity().fromJS(context, jsExecutor, arguments[0]);
					is.close();
					return null;
				}));
			});


			return context.eval("js",
				"""
						(async function*(openStreamFunc, fileName, streamPull, streamClose) {
							return {
								fileName: fileName ?? undefined,
								async *asBytes() {
									const stream = await openStreamFunc();
									try {
										for(;;) {
											const items = await streamPull(stream);
											if(items === null) {
												break;
											}
											yield* items;
										}
									}
									finally {
										await streamClose(stream);
									}
								},
							};
						})
					"""
			).execute(openStreamFunc, resource.fileName().orElse(null), streamPull, streamClose);
		}
		
	} 
}
