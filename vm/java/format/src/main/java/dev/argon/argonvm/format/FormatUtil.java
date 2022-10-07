package dev.argon.argonvm.format;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

final class FormatUtil {
	private FormatUtil() {}

	public static void writeIndex(OutputStream os, long index) throws IOException {
		do {
			byte nextPart = (byte)(index & 0x7F);
			index = index >>> 7;
			if(index != 0) {
				nextPart |= 0x80;
			}
			os.write(nextPart);
		} while(index != 0);
	}

	public static void writeInt16(OutputStream os, short value) throws IOException {
		os.write((byte)value);
		os.write(value >>> 8);
	}

	public static void writeInt8(OutputStream os, byte value) throws IOException {
		os.write(value);
	}

	public static byte readByte(InputStream is) throws IOException {
		int b = is.read();
		if(b == -1) {
			throw new EOFException();
		}

		return (byte)b;
	}

	public static long readIndex(InputStream is) throws IOException {
		long result = 0;
		byte b;
		int shift = 0;
		do {
			b = readByte(is);
			result = (result << shift) | (b & 0x7F);
			shift += 7;
		} while((b & 0x80) != 0);
		return result;
	}

	public static short readInt16(InputStream is) throws IOException {
		byte lo = readByte(is);
		byte hi = readByte(is);
		return (short)((lo & 0xFF) | ((hi & 0xFF) << 8));
	}

	public static byte readInt8(InputStream is) throws IOException {
		return readByte(is);
	}
}
