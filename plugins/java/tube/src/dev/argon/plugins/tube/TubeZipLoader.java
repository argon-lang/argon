package dev.argon.plugins.tube;

import java.util.List;
import dev.argon.plugin.api.*;
import dev.argon.plugin.api.tube.*;
import dev.argon.plugin.api.resource.*;
import dev.argon.plugin.api.util.InputStreamFormatReader;
import dev.argon.verilization.runtime.Codec;
import org.checkerframework.checker.nullness.qual.*;
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel;
import org.apache.commons.compress.archivers.zip.ZipFile;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.SeekableByteChannel;
import java.math.BigInteger;
import org.apache.commons.compress.utils.IOUtils;

final class TubeZipLoader<E extends Exception> implements TubeLoader<E, TubeOptions<E>> {
    @Override
    public List<String> supportedExtensions() {
        return List.of("artube");
    }

    @Override
    public CloseableTube<E> load(TubeOptions<E> options, BinaryResource<E> resource) throws IOException, E, PluginException, IOException {
        var channel = resource.asSeekableByteChannel();
        if(channel == null) {
            byte[] data;
            try(var is = resource.asInputStream()) {
                data = IOUtils.toByteArray(is);
            }

            channel = new SeekableInMemoryByteChannel(data);
        }
        try {
            return new CloseableZipTube<E>(channel);
        }
        catch(IOException ex) {
            channel.close();
            throw ex;
        }
    }

    private static final class CloseableZipTube<E extends Exception> implements CloseableTube<E> {
        public CloseableZipTube(SeekableByteChannel channel) throws IOException {
            this.channel = channel;
            tube = new ZipTube<E>(new ZipFile(channel));
        }

        private final SeekableByteChannel channel;
        private final SerializedTube<E> tube;

        @Override
        public SerializedTube<E> tube() {
            return tube;
        }
    
        @Override
        public void close() throws IOException, E {
            channel.close();
        }
    }

    private static final class ZipTube<E extends Exception> implements SerializedTube<E> {
        public ZipTube(ZipFile zip) {
            this.zip = zip;
        }

        private final ZipFile zip;

        private <T> T readEntry(String path, Codec<T> codec) throws IOException, E, PluginException {
            var entry = zip.getEntry(path);
            if(entry == null) {
                throw new TubeFormatException();
            }

            return codec.read(new InputStreamFormatReader(zip.getInputStream(entry)));
        }

        private <T> T readEntryExtern(String path, ExternCodec<E, T> codec) throws IOException, E, PluginException {
            var entry = zip.getEntry(path);
            if(entry == null) {
                throw new TubeFormatException();
            }

            var resource = new BinaryResource<E>() {
                @Override
                public InputStream asInputStream() throws IOException, E, PluginException {
                    return zip.getInputStream(entry);
                }
            };

            return codec.decode(resource);
        }

        @Override
        public Metadata.V1 metadata() throws IOException, E, PluginException {
            return readEntry("metadata", Metadata.V1.codec);
        }

        
        @Override
        public ModuleDeclaration.V1 moduleDeclaration(BigInteger id) throws IOException, E, PluginException {
            return readEntry("module/" + id, ModuleDeclaration.V1.codec);
        }

        @Override
        public ModuleDefinition.V1 moduleDefinition(BigInteger id) throws IOException, E, PluginException {
            return readEntry("module/def/" + id, ModuleDefinition.V1.codec);
        }


        @Override
        public TraitReference.V1 traitRef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("trait/ref/" + id, TraitReference.V1.codec);
        }
        
        @Override
        public TraitDefinition.V1 traitDef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("trait/" + id, TraitDefinition.V1.codec);
        }


        @Override
        public ClassReference.V1 classRef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("class/ref/" + id, ClassReference.V1.codec);
        }

        @Override
        public ClassDefinition.V1 classDef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("class/" + id, ClassDefinition.V1.codec);

        }

        
        @Override
        public FunctionReference.V1 functionRef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("function/ref/" + id, FunctionReference.V1.codec);
        }

        @Override
        public FunctionDefinition.V1 functionDef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("function/" + id, FunctionDefinition.V1.codec);
        }
        @Override
        public <ExternFunction> ExternFunction externFunctionImplementation(BigInteger id, Platform<E, ExternFunction, ?, ?> platform) throws IOException, E, PluginException {
            return readEntryExtern("function/extern/" + platform.id() + "/" + id, platform.externFunctionCodec());
        }


        @Override
        public MethodReference.V1 methodRef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("method/ref/" + id, MethodReference.V1.codec);
        }

        @Override
        public MethodDefinition.V1 methodDef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("method/" + id, MethodDefinition.V1.codec);
        }

        @Override
        public <ExternMethod> ExternMethod externMethodImplementation(BigInteger id, Platform<E, ?, ExternMethod, ?> platform) throws IOException, E, PluginException {
            return readEntryExtern("method/extern/" + platform.id() + "/" + id, platform.externMethodCodec());
        }

        
        @Override
        public ClassConstructorReference.V1 classConstructorRef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("class-ctor/ref/" + id, ClassConstructorReference.V1.codec);
        }

        @Override
        public ClassConstructorDefinition.V1 classConstructorDef(BigInteger id) throws IOException, E, PluginException {
            return readEntry("class-ctor/" + id, ClassConstructorDefinition.V1.codec);
        }
        
        @Override
        public <ExternClassCtor> ExternClassCtor externClassConstructorImplementation(BigInteger id, Platform<E, ?, ?, ExternClassCtor> platform) throws IOException, E, PluginException {
            return readEntryExtern("class-ctor/extern/" + platform.id() + "/" + id, platform.externClassConstructorCodec());
        }
    }
}
