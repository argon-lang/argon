package dev.argon.plugin.api.util;

import dev.argon.plugin.api.PluginOperations;
import org.jetbrains.annotations.NotNull;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Objects;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class ConnectedInputStream extends InputStream {

    public ConnectedInputStream(PluginOperations<?> pluginOperations) {
        this.pluginOperations = pluginOperations;
        lock = new ReentrantLock();
        dataReady = lock.newCondition();
        dataEmpty = lock.newCondition();
    }


    private final PluginOperations<?> pluginOperations;
    private final ReentrantLock lock;
    private final Condition dataReady;
    private final Condition dataEmpty;

    private boolean writerClosed = false;
    private boolean readerClosed = false;
    private Throwable writerError = null;
    private byte[] data = null;
    private int dataOff = 0;
    private int dataLen = 0;

    @Override
    public int read() throws IOException {
        byte[] b = new byte[1];
        int len = read(b);
        if(len == 0) {
            return -1;
        }
        else {
            return b[0];
        }
    }

    @Override
    public int read(byte @NotNull[] b, int off, int len) throws IOException {
        Objects.checkFromIndexSize(off, len, b.length);
        try {
            lock.lockInterruptibly();
            if(readerClosed) {
                throw new EOFException();
            }

            try {
                while(true) {
                    if(data == null) {
                        if(writerClosed) {
                            if(writerError != null) {
                                throw pluginOperations.catchAsIOException(() -> ConnectedInputStream.<RuntimeException>unsafeThrow(writerError));
                            }

                            return 0;
                        }

                        dataReady.await();
                    }
                    else {
                        len = Math.min(len, dataLen);
                        System.arraycopy(data, dataOff, b, off, len);
                        dataLen -= len;
                        dataOff += len;

                        if(dataLen == 0) {
                            data = null;
                            dataEmpty.signal();
                        }

                        return len;
                    }
                }
            }
            finally {
                lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw pluginOperations.wrapAsRuntimeException(ex);
        }
    }

    @Override
    public void close() throws IOException {
        try {
            lock.lockInterruptibly();
            try {
                readerClosed = true;
                dataEmpty.signal();
            }
            finally {
                lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw pluginOperations.wrapAsRuntimeException(ex);
        }
    }

    private static <T extends Throwable> T unsafeThrow(Throwable ex) throws T {
        @SuppressWarnings("unchecked")
        T ex2 = (T)ex;
        throw ex2;
    }

    final class ConnectedOutputStream extends OutputStream {
        @Override
        public void write(int b) throws IOException {
            write(new byte[] { (byte)b });
        }

        @Override
        public void write(byte @NotNull[] b, int off, int len) throws IOException {
            Objects.checkFromIndexSize(off, len, b.length);

            if(len == 0) {
                return;
            }

            try {
                lock.lockInterruptibly();
                if(writerClosed) {
                    throw new EOFException();
                }

                try {
                    while(true) {
                        if(readerClosed) {
                            throw new EOFException();
                        }

                        if(data != null) {
                            dataEmpty.await();
                            continue;
                        }

                        data = b;
                        dataOff = off;
                        dataLen = len;
                        dataReady.signal();
                        break;
                    }
                }
                finally {
                    lock.unlock();
                }
            }
            catch(InterruptedException ex) {
                throw pluginOperations.wrapAsRuntimeException(ex);
            }
        }

        public void setException(Throwable ex) throws InterruptedException {
            lock.lockInterruptibly();
            try {
                writerError = ex;
            }
            finally {
                lock.unlock();
            }
        }

        @Override
        public void close() {
            try {
                lock.lockInterruptibly();
                try {
                    writerClosed = true;
                    dataReady.signal();
                }
                finally {
                    lock.unlock();
                }
            }
            catch(InterruptedException ex) {
                throw pluginOperations.wrapAsRuntimeException(ex);
            }
        }
    }

}
