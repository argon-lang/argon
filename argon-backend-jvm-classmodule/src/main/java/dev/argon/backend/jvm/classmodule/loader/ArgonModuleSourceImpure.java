package dev.argon.backend.jvm.classmodule.loader;

import dev.argon.module.java_format.*;
import dev.argon.util.ExIterable;

public interface ArgonModuleSourceImpure {
    Metadata metadata() throws Exception;
    ModuleReferencesList references() throws Exception;

    ExIterable<NamespaceDeclaration> namespaces() throws Exception;
    ExIterable<GlobalDeclarationElement> namespaceElements(int id) throws Exception;

    TraitDefinition getTraitDef(int id) throws Exception;
    TraitReference getTraitRef(int id) throws Exception;

    ClassDefinition getClassDef(int id) throws Exception;
    ClassReference getClassRef(int id) throws Exception;

    DataConstructorDefinition getDataConstructorDef(int id) throws Exception;
    DataConstructorReference getDataConstructorRef(int id) throws Exception;

    FunctionDefinition getFunctionDef(int id) throws Exception;
    FunctionReference getFunctionRef(int id) throws Exception;

    MethodDefinition getMethodDef(int id) throws Exception;
    MethodReference getMethodRef(int id) throws Exception;

    ClassConstructorDefinition getClassConstructorDef(int id) throws Exception;
    ClassConstructorReference getClassConstructorRef(int id) throws Exception;
}
