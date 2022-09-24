package dev.argon.argonvm;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

public final class InstructionReader extends InputStream {
	InstructionReader(byte[] data) {
		this.data = data;
	}

	private final byte[] data;
	private int ip = 0;

	public int ip() {
		return ip;
	}

	@Override
	public int read() throws IOException {
		if(ip < data.length) {
			return data[ip++];
		}
		else {
			return -1;
		}
	}

	public void jump(int ip) {
		this.ip = ip;
	}

}
