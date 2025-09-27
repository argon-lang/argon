package dev.argon.backend.jsApi;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;

public final class GraalJavaScriptExecutor implements AutoCloseable {

    @HostAccess.Export
    public GraalJavaScriptExecutor(Map<String, String> files) {
        var fileSystem = new MemoryFileSystem(files);

        context = Context.newBuilder("js")
            .allowExperimentalOptions(true)
            .option("js.load", "false")
            .option("js.print", "false")
            .option("js.esm-eval-returns-exports", "true")
            .option("js.text-encoding", "true")
            .option("js.new-set-methods", "true")
            .option("engine.WarnInterpreterOnly", "false")
            .allowIO(
                IOAccess.newBuilder()
                    .fileSystem(fileSystem)
                    .build()
            )
            .out(outputStream)
            .err(outputStream)
            .in(InputStream.nullInputStream())
            .build();
    }

    private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    private final Context context;

    @HostAccess.Export
    public void executeScript(String code) throws IOException {
        var script = Source.newBuilder("js", code, null).build();
        context.eval(script);
    }

    @HostAccess.Export
    public void executeModule(String code) throws IOException {
        var module = Source.newBuilder("js", code, "main.js")
            .mimeType("application/javascript+module")
            .build();

        context.eval(module);
    }

    @HostAccess.Export
    public String output() {
        return outputStream.toString(StandardCharsets.UTF_8);
    }

    @HostAccess.Export
    @Override
    public void close() throws Exception {
        context.close();
    }

}
