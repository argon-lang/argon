package dev.argon.plugin.api_v1;

import dev.argon.module.java_format.*;
import dev.argon.plugin.api_v1.util.ExIterable;
import org.jetbrains.annotations.NotNull;

import java.io.InputStream;

public interface ArgonModuleSerialized extends AutoCloseable {
    @NotNull Metadata metadata() throws Exception;
    @NotNull ModuleReferencesList references() throws Exception;

    @NotNull ExIterable<NamespaceDeclaration> namespaces() throws Exception;
    @NotNull ExIterable<GlobalDeclarationElement> namespaceElements(int id) throws Exception;

    @NotNull TraitDefinition getTraitDef(int id) throws Exception;
    @NotNull TraitReference getTraitRef(int id) throws Exception;

    @NotNull ClassDefinition getClassDef(int id) throws Exception;
    @NotNull ClassReference getClassRef(int id) throws Exception;

    @NotNull DataConstructorDefinition getDataConstructorDef(int id) throws Exception;
    @NotNull DataConstructorReference getDataConstructorRef(int id) throws Exception;

    @NotNull FunctionDefinition getFunctionDef(int id) throws Exception;
    @NotNull FunctionReference getFunctionRef(int id) throws Exception;
    @NotNull InputStream getFunctionPayload(int id, String platformId) throws Exception;

    @NotNull MethodDefinition getMethodDef(int id) throws Exception;
    @NotNull MethodReference getMethodRef(int id) throws Exception;
    @NotNull InputStream getMethodPayload(int id, String platformId) throws Exception;

    @NotNull ClassConstructorDefinition getClassConstructorDef(int id) throws Exception;
    @NotNull ClassConstructorReference getClassConstructorRef(int id) throws Exception;
    @NotNull InputStream getClassConstructorPayload(int id, String platformId) throws Exception;
}
