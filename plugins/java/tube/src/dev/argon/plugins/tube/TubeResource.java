package dev.argon.plugins.tube;

import dev.argon.plugin.api.*;
import dev.argon.plugin.api.resource.*;
import dev.argon.plugin.api.tube.*;
import dev.argon.plugin.api.util.OutputStreamFormatWriter;
import dev.argon.plugin.api.util.OutputStreamWriter;
import dev.argon.verilization.runtime.Codec;
import java.util.Optional;
import java.util.Set;
import java.util.HashSet;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.utils.IOUtils;

abstract class TubeResource<E extends Exception> extends BinaryResource<E> {

    public TubeResource(Set<Platform<E, ?, ?, ?>> platforms) {
        this.platforms = platforms;
    }

    private final Set<Platform<E, ?, ?, ?>> platforms;

    public abstract SerializedTube<E> asTube() throws IOException, E;

    @Override
    public InputStream asInputStream() throws IOException, E {
        final class WriterHelper {
            public WriterHelper(ZipArchiveOutputStream zip, SerializedTube<E> tube) throws IOException, E, PluginException {
                this.zip = zip;
                this.tube = tube;
                metadata = tube.metadata();
            }

            private final ZipArchiveOutputStream zip;
            private final SerializedTube<E> tube;
            private final Metadata.V1 metadata;
            private final Set<BigInteger> writtenTraitRefs = new HashSet<>();
            private final Set<BigInteger> writtenClassRefs = new HashSet<>();
            private final Set<BigInteger> writtenFunctionRefs = new HashSet<>();
            private final Set<BigInteger> writtenMethodRefs = new HashSet<>();
            private final Set<BigInteger> writtenClassConstructorRefs = new HashSet<>();

            private <T> void writeEntry(String path, Codec<T> codec, T value) throws IOException, E, PluginException {
                var entry = new ZipArchiveEntry(path);
                zip.putArchiveEntry(entry);
                var writer = new OutputStreamFormatWriter(zip);
                codec.write(writer, value);
                zip.closeArchiveEntry();
            }

            public void write() throws IOException, E, PluginException {
                writeEntry("argon-tube-version", FormatVersion.V1.codec, CurrentFormatVersion.v1);

                writeEntry("metadata", Metadata.V1.codec, metadata);

                for(BigInteger i = BigInteger.ZERO; i.compareTo(metadata.moduleCount()) < 0; i = i.add(BigInteger.ONE)) {
                    writeModule(i);
                }
            }

            private Platform<E, ?, ?, ?> getPlatform(String id) {
                for(var platform : platforms) {
                    if(platform.id().equals(id)) {
                        return platform;
                    }
                }

                throw new IllegalArgumentException("Unknown platform: " + id);
            }

            private void writeModule(BigInteger id) throws IOException, E, PluginException {
                writeEntry("module/" + id, ModuleDeclaration.V1.codec, tube.moduleDeclaration(id));

                var moduleDef = tube.moduleDefinition(id);
                writeEntry("module/def/" + id, ModuleDefinition.V1.codec, moduleDef);

                for(int i = 0; i < moduleDef.exports().size(); ++i) {
                    switch(moduleDef.exports().get(i)) {
                        case ModuleExport.V1.Trait arTrait -> {
                            writeErasedSignatureParameterOnlyRefs(arTrait.trait().sig());
                            writeTraitDef(arTrait.trait().id());
                        }
                        case ModuleExport.V1.Class arClass -> {
                            writeErasedSignatureParameterOnlyRefs(arClass._class().sig());
                            writeClassDef(arClass._class().id());
                        }
                        case ModuleExport.V1.Function arFunc -> {
                            writeErasedSignatureRefs(arFunc.function().sig());
                            writeFunctionDef(arFunc.function().id());
                        }
                    }
                }
            }

            private void writeTraitDef(BigInteger id) throws IOException, E, PluginException {
                var traitDef = tube.traitDef(id);
                writeEntry("trait/" + id, TraitDefinition.V1.codec, traitDef);

                var sig = traitDef.signature();
                for(int i = 0; i < sig.variables().size(); ++i) {
                    writeLocalVariableRefs(sig.variables().get(i));
                }
                for(int i = 0; i < sig.parameters().size(); ++i) {
                    writeParameterRefs(sig.parameters().get(i));
                }
                for(int i = 0; i < sig.baseTraits().size(); ++i) {
                    writeTraitTypeRefs(sig.baseTraits().get(i));
                }

                for(int i = 0; i < traitDef.methods().size(); ++i) {
                    writeMethodDef(traitDef.methods().get(i).id());
                }

                for(int i = 0; i < traitDef.staticMethods().size(); ++i) {
                    writeMethodDef(traitDef.staticMethods().get(i).id());
                }
            }

            private void writeClassDef(BigInteger id) throws IOException, E, PluginException {
                var classDef = tube.classDef(id);
                writeEntry("class/" + id, ClassDefinition.V1.codec, classDef);

                var sig = classDef.signature();
                for(int i = 0; i < sig.variables().size(); ++i) {
                    writeLocalVariableRefs(sig.variables().get(i));
                }
                for(int i = 0; i < sig.parameters().size(); ++i) {
                    writeParameterRefs(sig.parameters().get(i));
                }
                if(sig.baseClass().isPresent()) {
                    writeClassTypeRefs(sig.baseClass().get());
                }
                for(int i = 0; i < sig.baseTraits().size(); ++i) {
                    writeTraitTypeRefs(sig.baseTraits().get(i));
                }

                for(int i = 0; i < classDef.fields().size(); ++i) {
                    writeExpressionWithVariableRefs(classDef.fields().get(i).fieldType());
                }

                for(int i = 0; i < classDef.methods().size(); ++i) {
                    writeMethodDef(classDef.methods().get(i).id());
                }

                for(int i = 0; i < classDef.staticMethods().size(); ++i) {
                    writeMethodDef(classDef.staticMethods().get(i).id());
                }

                for(int i = 0; i < classDef.constructors().size(); ++i) {
                    writeClassConstructorDef(classDef.constructors().get(i).id());
                }
            }

            private void writeFunctionDef(BigInteger id) throws IOException, E, PluginException {
                var funcDef = tube.functionDef(id);
                writeEntry("function/" + id, FunctionDefinition.V1.codec, funcDef);

                writeFunctionSignatureRefs(funcDef.signature());
                
                if(funcDef.body().isPresent()) {
                    switch(funcDef.body().get()) {
                        case FunctionBody.V1.ExpressionBody exprBody ->
                            writeExpressionWithVariableRefs(exprBody.expressionBody());
    
                        case FunctionBody.V1.ExternalImplementation external -> {
                            for(int i = 0; i < metadata.platforms().size(); ++i) {
                                var platform = getPlatform(metadata.platforms().get(i));
                                writeFunctionDefExternBody(id, platform);
                            }
                        }
                    }
                }
            }

            private <ExternFunction> void writeFunctionDefExternBody(BigInteger id, Platform<E, ExternFunction, ?, ?> platform) throws IOException, E, PluginException {
                String path = "function/extern/" + platform.id() + "/" + id;

                var bodyResource = platform.externFunctionCodec().encode(tube.externFunctionImplementation(id, platform));

                var entry = new ZipArchiveEntry(path);
                zip.putArchiveEntry(entry);
                try(var is = bodyResource.asInputStream()) {
                    IOUtils.copy(is, zip);
                }
                zip.closeArchiveEntry();
            }

            private void writeMethodDef(BigInteger id) throws IOException, E, PluginException {
                var methodDef = tube.methodDef(id);
                writeEntry("method/" + id, MethodDefinition.V1.codec, methodDef);

                writeFunctionSignatureRefs(methodDef.signature());
                
                if(methodDef.body().isPresent()) {
                    switch(methodDef.body().get()) {
                        case FunctionBody.V1.ExpressionBody exprBody ->
                            writeExpressionWithVariableRefs(exprBody.expressionBody());
    
                        case FunctionBody.V1.ExternalImplementation external -> {
                            for(var platform : platforms) {
                                writeMethodDefExternBody(id, platform);
                            }
                        }
                    }
                }
            }

            private <ExternMethod> void writeMethodDefExternBody(BigInteger id, Platform<E, ?, ExternMethod, ?> platform) throws IOException, E, PluginException {
                String path = "method/extern/" + platform.id() + "/" + id;

                var bodyResource = platform.externMethodCodec().encode(tube.externMethodImplementation(id, platform));

                var entry = new ZipArchiveEntry(path);
                zip.putArchiveEntry(entry);
                try(var is = bodyResource.asInputStream()) {
                    IOUtils.copy(is, zip);
                }
                zip.closeArchiveEntry();
            }

            private void writeClassConstructorDef(BigInteger id) throws IOException, E, PluginException {
                var ctor = tube.classConstructorDef(id);
                writeEntry("class-ctor/" + id, ClassConstructorDefinition.V1.codec, ctor);

                var sig = ctor.signature();
                for(int i = 0; i < sig.variables().size(); ++i) {
                    writeLocalVariableRefs(sig.variables().get(i));
                }
                for(int i = 0; i < sig.parameters().size(); ++i) {
                    writeParameterRefs(sig.parameters().get(i));
                }
                
                if(ctor.body().isPresent()) {
                    switch(ctor.body().get()) {
                        case ClassConstructorBody.V1.ExpressionBody exprBody ->
                            writeClassCtorBodyRefs(exprBody.expressionBody());

                        case ClassConstructorBody.V1.ExternalImplementation external -> {
                            for(var platform : platforms) {
                                writeClassCtorDefExternBody(id, platform);
                            }
                        }
                    }
                }
            }

            private <ExternClassCtor> void writeClassCtorDefExternBody(BigInteger id, Platform<E, ?, ?, ExternClassCtor> platform) throws IOException, E, PluginException {
                String path = "class-ctor/extern/" + platform.id() + "/" + id;

                var bodyResource = platform.externClassConstructorCodec().encode(tube.externClassConstructorImplementation(id, platform));

                var entry = new ZipArchiveEntry(path);
                zip.putArchiveEntry(entry);
                try(var is = bodyResource.asInputStream()) {
                    IOUtils.copy(is, zip);
                }
                zip.closeArchiveEntry();
            }

            private void writeErasedSignatureParameterOnlyRefs(ErasedSignatureParameterOnly.V1 sig) throws IOException, E, PluginException {
                for(int i = 0; i < sig.parameterTypes().size(); ++i) {
                    writeSigTypeRefs(sig.parameterTypes().get(i));
                }
            }

            private void writeErasedSignatureRefs(ErasedSignature.V1 sig) throws IOException, E, PluginException {
                for(int i = 0; i < sig.parameterTypes().size(); ++i) {
                    writeSigTypeRefs(sig.parameterTypes().get(i));
                }

                writeSigTypeRefs(sig.resultType());
            }

            private void writeSigTypeRefs(SigType.V1 sigType) throws IOException, E, PluginException {
                switch(sigType) {
                    case SigType.V1.Erased erased -> {}
                    case SigType.V1.Class cls -> writeSigTypeClassRefs(cls);
                    case SigType.V1.Trait trait -> writeSigTypeTraitRefs(trait);
                    case SigType.V1.Tuple tuple -> writeSigTypeTupleRefs(tuple);
                    case SigType.V1.Function function -> {
                        writeSigTypeRefs(function.function().argumentType());
                        writeSigTypeRefs(function.function().resultType());
                    }
                }
            }

            private void writeSigTypeClassRefs(SigType.V1.Class cls) throws IOException, E, PluginException {
                var id = cls._class().id();
                if(id.compareTo(BigInteger.ZERO) < 0) {
                    writeClassRef(id.negate().subtract(BigInteger.ONE));
                }
                
                for(int i = 0; i < cls._class().arguments().size(); ++i) {
                    writeSigTypeRefs(cls._class().arguments().get(i));
                }
            }

            private void writeSigTypeTraitRefs(SigType.V1.Trait trait) throws IOException, E, PluginException {
                var id = trait.trait().id();
                if(id.compareTo(BigInteger.ZERO) < 0) {
                    writeTraitRef(id.negate().subtract(BigInteger.ONE));
                }
                
                for(int i = 0; i < trait.trait().arguments().size(); ++i) {
                    writeSigTypeRefs(trait.trait().arguments().get(i));
                }
            }

            private void writeSigTypeTupleRefs(SigType.V1.Tuple tuple) throws IOException, E, PluginException {
                for(int i = 0; i < tuple.tuple().arguments().size(); ++i) {
                    writeSigTypeRefs(tuple.tuple().arguments().get(i));
                }
            }

            private void writeLocalVariableRefs(LocalVariableDeclaration.V1 local) throws IOException, E, PluginException {
                writeExpressionRefs(local.varType());
            }

            private void writeParameterRefs(Parameter.V1 parameter) throws IOException, E, PluginException {
                writeExpressionRefs(parameter.type());
            }

            private void writeClassTypeRefs(ClassType.V1 classType) throws IOException, E, PluginException {
                var id = classType.id();
                if(id.compareTo(BigInteger.ZERO) < 0) {
                    writeClassRef(id.negate().subtract(BigInteger.ONE));
                }

                for(int i = 0; i < classType.arguments().size(); ++i) {
                    writeExpressionRefs(classType.arguments().get(i));
                }
            }

            private void writeTraitTypeRefs(TraitType.V1 traitType) throws IOException, E, PluginException {
                var id = traitType.id();
                if(id.compareTo(BigInteger.ZERO) < 0) {
                    writeTraitRef(id.negate().subtract(BigInteger.ONE));
                }

                for(int i = 0; i < traitType.arguments().size(); ++i) {
                    writeExpressionRefs(traitType.arguments().get(i));
                }
            }

            private void writeFunctionSignatureRefs(FunctionSignature.V1 sig) throws IOException, E, PluginException {
                for(int i = 0; i < sig.variables().size(); ++i) {
                    writeLocalVariableRefs(sig.variables().get(i));
                }

                for(int i = 0; i < sig.parameters().size(); ++i) {
                    writeParameterRefs(sig.parameters().get(i));
                }

                writeExpressionRefs(sig.returnType());
            }

            private void writeExpressionWithVariableRefs(ExpressionWithVariables.V1 expr) throws IOException, E, PluginException {
                for(int i = 0; i < expr.variables().size(); ++i) {
                    writeLocalVariableRefs(expr.variables().get(i));
                }

                writeExpressionRefs(expr.expression());
            }

            private void writeClassCtorBodyRefs(ClassConstructorExpressionBody.V1 body) throws IOException, E, PluginException {
                for(int i = 0; i < body.variables().size(); ++i) {
                    writeLocalVariableRefs(body.variables().get(i));
                }

                for(int i = 0; i < body.preInit().size(); ++i) {
                    var stmt = body.preInit().get(i);
                    switch(stmt) {
                        case PreInitClassConstructorStatement.V1.Expression expr ->
                            writeExpressionRefs(expr.expression());

                        case PreInitClassConstructorStatement.V1.FieldInitializer fieldInit ->
                            writeExpressionWithVariableRefs(fieldInit.fieldInitializer().value());
                    }
                    
                }

                if(body.baseConstructorCall().isPresent()) {
                    var baseCall = body.baseConstructorCall().get();
                    for(int i = 0; i < baseCall.args().size(); ++i) {
                        writeExpressionRefs(baseCall.args().get(i));
                    }
                }

                writeExpressionRefs(body.endExpr());
            }

            private void writeExpressionRefs(Expression.V1 expr) throws IOException, E, PluginException {
                if(expr.constructor() instanceof ExpressionConstructor.V1.ClassConstructorCall call) {
                    if(call.classConstructorCall().compareTo(BigInteger.ZERO) < 0) {
                        writeTraitRef(call.classConstructorCall().negate().subtract(BigInteger.ONE));
                    }
                }
                else if(expr.constructor() instanceof ExpressionConstructor.V1.FunctionCall call) {
                    if(call.functionCall().compareTo(BigInteger.ZERO) < 0) {
                        writeFunctionRef(call.functionCall().negate().subtract(BigInteger.ONE));
                    }
                }
                else if(expr.constructor() instanceof ExpressionConstructor.V1.MethodCall call) {
                    if(call.methodCall().compareTo(BigInteger.ZERO) < 0) {
                        writeMethodRef(call.methodCall().negate().subtract(BigInteger.ONE));
                    }
                }
                else if(expr.constructor() instanceof ExpressionConstructor.V1.TraitType traitType) {
                    if(traitType.traitType().compareTo(BigInteger.ZERO) < 0) {
                        writeTraitRef(traitType.traitType().negate().subtract(BigInteger.ONE));
                    }
                } 
                else if(expr.constructor() instanceof ExpressionConstructor.V1.ClassType classType) {
                    if(classType.classType().compareTo(BigInteger.ZERO) < 0) {
                        writeTraitRef(classType.classType().negate().subtract(BigInteger.ONE));
                    }
                }

                for(int i = 0; i < expr.args().size(); ++i) {
                    writeExpressionRefs(expr.args().get(i));
                }
            }

            private void writeTraitRef(BigInteger id) throws IOException, E, PluginException {
                if(!writtenTraitRefs.add(id)) return;
                var traitRef = tube.traitRef(id);
                writeEntry("trait/ref/" + id, TraitReference.V1.codec, traitRef);
                writeErasedSignatureParameterOnlyRefs(traitRef.signature());
            }

            private void writeClassRef(BigInteger id) throws IOException, E, PluginException {
                if(!writtenClassRefs.add(id)) return;
                var classRef = tube.classRef(id);
                writeEntry("class/ref/" + id, ClassReference.V1.codec, classRef);
                writeErasedSignatureParameterOnlyRefs(classRef.signature());
            }

            private void writeFunctionRef(BigInteger id) throws IOException, E, PluginException {
                if(!writtenFunctionRefs.add(id)) return;
                var funcRef = tube.functionRef(id);
                writeEntry("function/ref/" + id, FunctionReference.V1.codec, funcRef);
                writeErasedSignatureRefs(funcRef.signature());
            }

            private void writeMethodRef(BigInteger id) throws IOException, E, PluginException {
                if(!writtenMethodRefs.add(id)) return;
                var methodRef = tube.methodRef(id);
                writeEntry("method/ref/" + id, MethodReference.V1.codec, methodRef);
                writeErasedSignatureRefs(methodRef.signature());
            }

            private void writeClassCtorRef(BigInteger id) throws IOException, E, PluginException {
                if(!writtenClassConstructorRefs.add(id)) return;
                var classCtorRef = tube.classConstructorRef(id);
                writeEntry("class-ctor/ref/" + id, ClassConstructorReference.V1.codec, classCtorRef);
                writeErasedSignatureParameterOnlyRefs(classCtorRef.signature());
                writeClassRef(classCtorRef.ownerClass());
            }
        };

        var is = new OutputStreamWriter<E>() {
            @Override
            public void writer(OutputStream os) throws IOException, E, PluginException {
                try(var zip = new ZipArchiveOutputStream(os)) {
                    var tube = asTube();
                    new WriterHelper(zip, tube).write();
                }      
            }
        };
        is.launchWriter();
        return is;
    }
}
