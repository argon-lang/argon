package dev.argon.backend.jvm.classmodule.loader;

import dev.argon.module.java_format.*;

public interface JavaArModuleLoader {
    Metadata getMetadata() throws Exception;

    TraitDefinition getTraitDef(int id) throws Exception;
    TraitReference getTraitRef(int id) throws Exception;
    ClassDefinition getClassDef(int id) throws Exception;
    ClassReference getClassRef(int id) throws Exception;
    DataConstructorDefinition getDataCtorDef(int id) throws Exception;
    DataConstructorReference getDataCtorRef(int id) throws Exception;
    FunctionDefinition getFuncDef(int id) throws Exception;
    FunctionReference getFuncRef(int id) throws Exception;
    MethodDefinition getMethodDef(int id) throws Exception;
    MethodReference getMethodRef(int id) throws Exception;
    ClassConstructorDefinition getClassCtorDef(int id) throws Exception;
    ClassConstructorReference getClassCtorRef(int id) throws Exception;
}
