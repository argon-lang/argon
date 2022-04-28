package dev.argon.plugin.api.util;

import dev.argon.verilization.runtime.FormatReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.EOFException;

public final class InputStreamFormatReader implements FormatReader {

    public InputStreamFormatReader(InputStream inputStream) {
        this.inputStream = inputStream;
        
    }

    private final InputStream inputStream;

    @Override
    public byte readByte() throws IOException {
        var b = inputStream.readNBytes(1);
        return b[0];
    }

    @Override
    public short readShort() throws IOException {
        var b = inputStream.readNBytes(2);
        return (short)((b[0] & 0xFF) | (b[1] & 0xFF) << 8);
    }

    @Override
    public int readInt() throws IOException {
        var b = inputStream.readNBytes(4);
        return (b[0] & 0xFF) | (b[1] & 0xFF) << 8 | (b[2] & 0xFF) << 16 | (b[3] & 0xFF) << 24;
    }

    @Override
    public long readLong() throws IOException {
        var b = inputStream.readNBytes(8);
        return (b[0] & 0xFFL) | (b[1] & 0xFFL) << 8 | (b[2] & 0xFFL) << 16 | (b[3] & 0xFFL) << 24 |
          (b[4] & 0xFFL) << 32 | (b[5] & 0xFFL) << 40 | (b[6] & 0xFFL) << 48 | (b[7] & 0xFFL) << 56;
    }

    @Override
    public byte[] readBytes(int count) throws IOException {
        return inputStream.readNBytes(count);
    }
}
