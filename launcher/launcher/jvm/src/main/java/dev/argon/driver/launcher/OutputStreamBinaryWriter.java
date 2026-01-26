package dev.argon.driver.launcher;

import dev.argon.backend.api.BinaryWriter;
import dev.argon.nobleidl.runtime.ErrorType;
import org.eclipse.collections.api.list.primitive.ImmutableByteList;
import org.jspecify.annotations.NonNull;

import java.io.IOException;
import java.io.OutputStream;

class OutputStreamBinaryWriter implements BinaryWriter<IOException> {
	public OutputStreamBinaryWriter(OutputStream os) {
		this.os = os;
	}

	private final OutputStream os;

	@Override
	public <E_E extends Throwable> void write(ErrorType<IOException, ? extends E_E> errorType_e, @NonNull ImmutableByteList bytes) throws InterruptedException, E_E {
		try {
			os.write(bytes.toArray());
		} catch(IOException e) {
			throw errorType_e.toThrowable(e);
		}
	}
}
