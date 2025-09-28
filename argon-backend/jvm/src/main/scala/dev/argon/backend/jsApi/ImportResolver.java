package dev.argon.backend.jsApi;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.proxy.ProxyObject;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

final class ImportResolver {
    private ImportResolver() { }
    

    public static Map<String, String> resolveModules(String entrypoint, Map<String, String> files) {
        Map<String, String> moduleFiles = new HashMap<>(files);

        try(
            var context = Context.newBuilder("js")
                .allowExperimentalOptions(true)
                .option("js.load", "false")
                .option("js.print", "false")
                .option("js.esm-eval-returns-exports", "true")
                .option("js.text-encoding", "true")
                .option("js.new-set-methods", "true")
                .option("engine.WarnInterpreterOnly", "false")
                .build()
        ) {
            var polyfillUrl = ImportResolver.class.getResource("polyfill.js");
            if(polyfillUrl == null) {
                throw new RuntimeException("Could not find polyfill.js");
            }

            var polyfillSrc = Source.newBuilder("js", polyfillUrl).build();
            context.eval(polyfillSrc);

            var resolverUrl = ImportResolver.class.getResource("import-resolver.js");
            if(resolverUrl == null) {
                throw new RuntimeException("Could not find import-resolver.js");
            }

            var resolverSrc = Source.newBuilder("js", resolverUrl)
                .mimeType("application/javascript+module")
                .build();

            var resolver = context.eval(resolverSrc);
            var updatedFiles = resolver.invokeMember(
                "resolveImports",
                MemoryFileSystem.getFullPath(Path.of("/backend", entrypoint)),
                ProxyObject.fromMap(new HashMap<>(files))
            );
            
            for(var member : updatedFiles.getMemberKeys()) {
                moduleFiles.put(member, updatedFiles.getMember(member).asString());
            }
            
            return moduleFiles;
        }
        catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }


}
