package dev.argon.plugin.api.util;

import dev.argon.plugin.api.PluginOperations;

import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterators;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public abstract class StreamGenerator<E extends Throwable, A> {
    public StreamGenerator(PluginOperations<E> pluginOperations) {
        this.pluginOperations = pluginOperations;

        lock = new ReentrantLock();
        itemReady = lock.newCondition();
        itemConsumed = lock.newCondition();
    }

    private final PluginOperations<E> pluginOperations;

    private final ReentrantLock lock;
    private final Condition itemReady;
    private final Condition itemConsumed;
    private boolean isCanceled = false;
    private boolean isDone = false;
    private boolean hasItem = false;
    private A currentItem = null;
    private Throwable generatorError = null;

    public abstract void generate() throws E, IOException, InterruptedException;

    protected final void emit(A item) throws InterruptedException {
        lock.lockInterruptibly();
        try {
            while(true) {
                if(isCanceled) {
                    throw new InterruptedException();    
                }
                
                if(hasItem) {
                    itemConsumed.await();
                    continue;
                }

                hasItem = true;
                currentItem = item;
                itemReady.notify();
                break;
            }
        }
        finally {
            lock.unlock();
        }
    }

    public Stream<A> asStream() {
        new Thread(() -> {
            try {
                try {
                    generate();
                }
                catch(InterruptedException ex) {
                    if(!isCanceled) {
                        throw ex;
                    }
                }
            }
            catch(Throwable ex) {
                generatorError = ex;
            }
            finally {
                lock.lock();
                try {
                    isDone = true;
                    itemReady.notify();
                }
                finally {
                    lock.unlock();
                }
            }
        }).start();

        Iterator<A> iter = new Iterator<A>() {
            @Override
            public boolean hasNext() {
                try {
                    lock.lockInterruptibly();
                    try {
                        while(true) {
                            if(isDone) {
                                return generatorError != null;
                            }

                            if(!hasItem) {
                                itemReady.await();
                                continue;
                            }

                            return true;
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
            public A next() {
                return pluginOperations.catchAsRuntimeException(() -> {
                    lock.lockInterruptibly();
                    try {
                        while(true) {
                            if(isDone) {
                                if(generatorError != null) {
                                    throw StreamGenerator.<RuntimeException>unsafeThrow(generatorError);
                                }
                                else {
                                    throw new NoSuchElementException();
                                }
                            }

                            if(!hasItem) {
                                itemReady.await();
                                continue;
                            }

                            var item = currentItem;
                            currentItem = null;
                            hasItem = false;
                            itemConsumed.notify();
                            return item;
                        }
                    }
                    finally {
                        lock.unlock();
                    }
                });
            }
        };

        return StreamSupport
            .stream(Spliterators.spliteratorUnknownSize(iter, 0), false)
            .onClose(() -> {
                lock.lock();
                try {
                    isCanceled = true;
                    itemConsumed.notify();
                }
                finally {
                    lock.unlock();
                }
            });
    }

    private static <T extends Throwable> T unsafeThrow(Throwable ex) throws T {
        @SuppressWarnings("unchecked")
        T ex2 = (T)ex;
        throw ex2;
    }
}
