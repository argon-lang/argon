package dev.argon.argonvm.engine;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public final class WasmMemory {
	public WasmMemory(int pages) {
		buffer = ByteBuffer.allocate(pages * 65536).order(ByteOrder.LITTLE_ENDIAN);
	}

	private final ByteBuffer buffer;


	public void storeI32(int addr, int value, byte align, int offset) {
		buffer.putInt(addr, value + offset);
	}

	public void storeBuffer(int addr, byte[] data) {
		buffer.put(addr, data);
	}

	public int loadI32(int addr, byte align, int offset) {
		return buffer.getInt(addr + offset);
	}

	public byte[] loadBuffer(int offset, int length) {
		byte[] b = new byte[length];
		buffer.get(offset, b);
		return b;
	}

}
