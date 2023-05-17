package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.DirectoryResource;
import dev.argon.plugin.api.FileSystemResource;
import org.graalvm.polyglot.Value;

final class WrapFileSystemResource {
    private WrapFileSystemResource() {}
    
    public static <E extends Throwable> Value wrap(JSEnv<E> env, FileSystemResource<E> res) {
        env.lock.lock();
        try {
            if(res instanceof BinaryResource<E> binRes) {
                return env.context.asValue(new WrapBinaryResource<>(env, binRes));
            }
            else if(res instanceof DirectoryResource<E, ?> dirRes) {
                return env.context.asValue(new WrapDirectoryResource<>(env, dirRes));
            }
            else {
                throw new RuntimeException("Unexpected resource");
            }
        }
        finally {
            env.lock.unlock();
        }
    }
}
