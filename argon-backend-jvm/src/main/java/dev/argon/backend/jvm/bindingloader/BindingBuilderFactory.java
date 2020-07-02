package dev.argon.backend.jvm.bindingloader;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.objectweb.asm.signature.SignatureReader;

public interface BindingBuilderFactory {

    @NotNull BindingBuilder create(int access, @NotNull String name, @NotNull String signature, @NotNull String superName, @NotNull String[] interfaces);

    @NotNull FieldBuilder createFieldBuilder(int access, @NotNull String name, @NotNull String descriptor,  @NotNull SignatureReader signature, @Nullable Object value);
    @NotNull MethodBuilder createMethodBuilder(int access, @NotNull String name, @NotNull String descriptor, @NotNull SignatureReader signature);
}
