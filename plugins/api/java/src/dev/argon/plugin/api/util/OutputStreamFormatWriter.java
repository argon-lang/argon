package dev.argon.plugin.api.util;

import dev.argon.verilization.runtime.FormatWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.EOFException;

public final class OutputStreamFormatWriter implements FormatWriter {

    public OutputStreamFormatWriter(OutputStream outputStream) {
        this.outputStream = outputStream;
    }

    private final OutputStream outputStream;

    @Override
    public void writeByte(byte b) throws IOException {
        outputStream.write(b);
    }

    @Override
    public void writeShort(short s) throws IOException {
        outputStream.write(s);
        outputStream.write(s >> 8);
    }

    @Override
    public void writeInt(int i) throws IOException {
        outputStream.write(i);
        outputStream.write(i >> 8);
        outputStream.write(i >> 16);
        outputStream.write(i >> 24);
    }

    @Override
    public void writeLong(long l) throws IOException {
        outputStream.write((int)l);
        outputStream.write((int)(l >> 8));
        outputStream.write((int)(l >> 16));
        outputStream.write((int)(l >> 24));
        outputStream.write((int)(l >> 32));
        outputStream.write((int)(l >> 40));
        outputStream.write((int)(l >> 48));
        outputStream.write((int)(l >> 56));
    }

    @Override
    public void writeBytes(byte[] data) throws IOException {
        outputStream.write(data);
    }

    @Override
    public void flush() throws IOException {
        outputStream.flush();
    }

}
