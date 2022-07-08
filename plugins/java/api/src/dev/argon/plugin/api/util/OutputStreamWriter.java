package dev.argon.plugin.api.util;

import dev.argon.plugin.api.PluginException;
import java.util.Objects;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.EOFException;
import org.checkerframework.checker.nullness.qual.*;

public abstract class OutputStreamWriter<E extends Exception> extends InputStream {
    
    private Object lock = new Object();
    private @Nullable Thread writeThread;
    private boolean isDone = false;
    private @Nullable Throwable ex = null;
    private byte @Nullable[] data = null;
    private int dataOff;
    private int dataLen;


    protected abstract void writer(OutputStream os) throws IOException, E, PluginException;

    @Override
    public final int read() throws IOException {
        byte[] b = new byte[1];
        read(b);
        return b[0];
    }

    @Override
    public final int read(byte b[], int off, int len) throws IOException {
        synchronized(lock) {
            while(data == null) {
                if(ex != null) {
                    throw this.<RuntimeException>unsafeThrow(ex);
                }
                else if(isDone) {
                    return 0;
                }
                else {
                    try {
                        lock.wait();
                    }
                    catch(InterruptedException ex) {
                        throw this.<RuntimeException>unsafeThrow(ex);
                    }
                }
            }

            int bytesRead = Math.min(len, dataLen);
            System.arraycopy(data, dataOff, b, off, bytesRead);

            if(bytesRead < dataLen) {
                dataOff += bytesRead;
                dataLen -= bytesRead;
            }
            else {
                data = null;
            }

            lock.notify();

            return bytesRead;
        }
    }

    @Override
    public void close() throws IOException {
        synchronized(lock) {
            isDone = true;
            if(writeThread != null) {
                writeThread.interrupt();
            }
        }
    }

    private void runWriter() {
        OutputStream os = new OutputStream() {
            @Override
            public void write(int b) throws IOException {
                write(new byte[] { (byte)b });
            }

            @Override
            public void write(byte[] b, int off, int len) throws IOException {
                Objects.checkFromIndexSize(off, len, b.length);

                synchronized(lock) {
                    while(data != null) {
                        if(isDone) {
                            throw new EOFException();
                        }
                        
                        try {
                            lock.wait();
                        }
                        catch(InterruptedException ex) {
                            throw new EOFException();
                        }
                    }

                    data = b;
                    dataOff = off;
                    dataLen = len;
                    lock.notify();
                }
            }
        };

        try {
            writer(os);
        }
        catch(Throwable ex) {
            synchronized(lock) {
                isDone = true;
                this.ex = ex;
            }
        }
        finally {
            synchronized(lock) {
                isDone = true;
                lock.notify();
            }
        }
    }


    public final void launchWriter() {
        writeThread = new Thread(this::runWriter);
        writeThread.start();
    }

    private <E2 extends Throwable> E2 unsafeThrow(Throwable ex) throws E2 {
        @SuppressWarnings("unchecked")
        E2 ex2 = (E2)ex;

        throw ex2;
    }

}
