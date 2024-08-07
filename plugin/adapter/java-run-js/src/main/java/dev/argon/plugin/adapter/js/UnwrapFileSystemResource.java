package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.FileSystemResource;
import org.graalvm.polyglot.Value;

import java.io.IOException;

final class UnwrapFileSystemResource {
    private UnwrapFileSystemResource() {}
    
    public static <E extends Throwable> FileSystemResource<E> unwrap(JSEnv<E> env, Value res) throws InterruptedException {
        env.lock.lockInterruptibly();
        try {
            return switch(res.getMember("resourceType").asString()) {
                case "binary" -> new UnwrapBinaryResource<>(env, res);
                case "directory" -> new UnwrapDirectoryResource<>(env, res);
                default -> throw new RuntimeException("Unknown resourceType");
            };
        }
        finally {
            env.lock.unlock();
        }
    }
}
