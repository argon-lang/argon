package dev.argon.plugins.tube;

import dev.argon.plugin.api.tube.*;
import dev.argon.plugin.api.*;
import dev.argon.verilization.runtime.Unit;
import java.math.BigInteger;
import java.io.IOException;

final class TubeInterface<E extends Exception> implements SerializedTube<E> {
    public TubeInterface(SerializedTube<E> tube) {
        this.tube = tube;
    }

    private final SerializedTube<E> tube;


    @Override
    public Metadata.V1 metadata() throws IOException, E, PluginException {
        var metadata = tube.metadata();
        return new Metadata.V1(
            metadata.name(),
            new TubeType.V1.Interface(Unit.INSTANCE),
            metadata.platforms(),
            metadata.references(),
            metadata.moduleCount()
        );
    }

    @Override
    public ModuleDeclaration.V1 moduleDeclaration(BigInteger id) throws IOException, E, PluginException {
        return tube.moduleDeclaration(id);
    }
    @Override
    public ModuleDefinition.V1 moduleDefinition(BigInteger id) throws IOException, E, PluginException {
        return tube.moduleDefinition(id);
    }


    @Override
    public TraitReference.V1 traitRef(BigInteger id) throws IOException, E, PluginException {
        return tube.traitRef(id);
    }
    @Override
    public TraitDefinition.V1 traitDef(BigInteger id) throws IOException, E, PluginException {
        return tube.traitDef(id);
    }

    @Override
    public ClassReference.V1 classRef(BigInteger id) throws IOException, E, PluginException {
        return tube.classRef(id);
    }
    @Override
    public ClassDefinition.V1 classDef(BigInteger id) throws IOException, E, PluginException {
        return tube.classDef(id);
    }

    @Override
    public FunctionReference.V1 functionRef(BigInteger id) throws IOException, E, PluginException {
        return tube.functionRef(id);
    }
    @Override
    public FunctionDefinition.V1 functionDef(BigInteger id) throws IOException, E, PluginException {
        return tube.functionDef(id);
    }
    @Override
    public <ExternFunction> ExternFunction externFunctionImplementation(BigInteger id, Platform<E, ExternFunction, ?, ?> platform) throws IOException, E, PluginException {
        return tube.externFunctionImplementation(id, platform);
    }

    @Override
    public MethodReference.V1 methodRef(BigInteger id) throws IOException, E, PluginException {
        return tube.methodRef(id);
    }
    @Override
    public MethodDefinition.V1 methodDef(BigInteger id) throws IOException, E, PluginException {
        return tube.methodDef(id);
    }
    @Override
    public <ExternMethod> ExternMethod externMethodImplementation(BigInteger id, Platform<E, ?, ExternMethod, ?> platform) throws IOException, E, PluginException {
        return tube.externMethodImplementation(id, platform);
    }

    @Override
    public ClassConstructorReference.V1 classConstructorRef(BigInteger id) throws IOException, E, PluginException {
        return tube.classConstructorRef(id);
    }
    @Override
    public ClassConstructorDefinition.V1 classConstructorDef(BigInteger id) throws IOException, E, PluginException {
        return tube.classConstructorDef(id);
    }
    @Override
    public <ExternClassCtor> ExternClassCtor externClassConstructorImplementation(BigInteger id, Platform<E, ?, ?, ExternClassCtor> platform) throws IOException, E, PluginException {
        return tube.externClassConstructorImplementation(id, platform);
    }

}
