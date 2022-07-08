package dev.argon.plugin.api.tube;

import java.math.BigInteger;
import dev.argon.plugin.api.*;
import java.io.IOException;

public interface SerializedTube<E extends Exception> {
    Metadata.V1 metadata() throws E, IOException, PluginException;

    ModuleDeclaration.V1 moduleDeclaration(BigInteger id) throws E, IOException, PluginException;
    ModuleDefinition.V1 moduleDefinition(BigInteger id) throws E, IOException, PluginException;


    TraitReference.V1 traitRef(BigInteger id) throws E, IOException, PluginException;
    TraitDefinition.V1 traitDef(BigInteger id) throws E, IOException, PluginException;

    ClassReference.V1 classRef(BigInteger id) throws E, IOException, PluginException;
    ClassDefinition.V1 classDef(BigInteger id) throws E, IOException, PluginException;

    FunctionReference.V1 functionRef(BigInteger id) throws E, IOException, PluginException;
    FunctionDefinition.V1 functionDef(BigInteger id) throws E, IOException, PluginException;
    <ExternFunction> ExternFunction externFunctionImplementation(BigInteger id, Platform<E, ExternFunction, ?, ?> platform) throws E, IOException, PluginException;

    MethodReference.V1 methodRef(BigInteger id) throws E, IOException, PluginException;
    MethodDefinition.V1 methodDef(BigInteger id) throws E, IOException, PluginException;
    <ExternMethod> ExternMethod externMethodImplementation(BigInteger id, Platform<E, ?, ExternMethod, ?> platform) throws E, IOException, PluginException;

    ClassConstructorReference.V1 classConstructorRef(BigInteger id) throws E, IOException, PluginException;
    ClassConstructorDefinition.V1 classConstructorDef(BigInteger id) throws E, IOException, PluginException;
    <ExternClassCtor> ExternClassCtor externClassConstructorImplementation(BigInteger id, Platform<E, ?, ?, ExternClassCtor> platform) throws E, IOException, PluginException;
    
}
