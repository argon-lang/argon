package dev.argon.testrunner;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.locks.ReentrantLock;

class TestExecutor {
    
    private final ReentrantLock lock = new ReentrantLock();
    private final List<Thread> threads = new ArrayList<>();

    public Future<Void> submit(Runnable command) {
        return submit(() -> {
            command.run();
            return null;
        });
    }
    
    public <T> Future<T> submit(Callable<T> command) {
        var future = new CompletableFuture<T>();
        
        lock.lock();
        try {
            var thread = Thread.startVirtualThread(() -> {
                try {
                    T result;
                    try {
                        result = command.call();
                    }
                    finally {
                        lock.lockInterruptibly();
                        try {
                            threads.remove(Thread.currentThread());
                        }
                        finally {
                            lock.unlock();
                        }
                    }
                    future.complete(result);
                }
                catch(Throwable e) {
                    future.completeExceptionally(e);
                }
            });
            threads.add(thread);
        }
        finally {
            lock.unlock();
        }
        
        return future;
    }
    
    public void shutdown() {
        lock.lock();
        try {
            threads.forEach(Thread::interrupt);
        }
        finally {
            lock.unlock();
        }
    }
    
    
}
