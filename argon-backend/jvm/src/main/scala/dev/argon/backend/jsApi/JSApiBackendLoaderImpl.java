package dev.argon.backend.jsApi;

import dev.argon.backend.api.Backend;
import dev.argon.backend.api.BackendFactory;
import dev.argon.backend.api.HostOperations;
import dev.argon.nobleidl.runtime.ErrorType;
import dev.argon.nobleidl.runtime.graaljsInterop.ErrorTypeAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSExecutor;
import org.apache.commons.text.StringEscapeUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Map;

class JSApiBackendLoaderImpl implements AutoCloseable {
    public JSApiBackendLoaderImpl(
        Map<String, String> files,
        String importPath,
        String exportName
    ) {
        this.importPath = importPath;
        this.exportName = exportName;
        var executor = Executors.newSingleThreadExecutor();
        var javaExecutor = Executors.newVirtualThreadPerTaskExecutor();

        jsExecutor = JSExecutor.fromExecutors(executor, javaExecutor);

        jsContext = Context.newBuilder("js")
            .allowHostClassLookup(_ -> true)
            .allowHostAccess(HostAccess.ALL)
            .allowExperimentalOptions(true)
            .allowIO(
                IOAccess.newBuilder()
                    .fileSystem(new MemoryFileSystem(files))
                    .build()
            )
            .option("js.load", "false")
            .option("js.print", "false")
            .option("js.esm-eval-returns-exports", "true")
            .option("js.text-encoding", "true")
            .option("js.new-set-methods", "true")
            .option("engine.WarnInterpreterOnly", "false")
            .build();
    }

    private final String importPath;
    private final String exportName;
    private final JSExecutor jsExecutor;
    private final Context jsContext;

    private final ReentrantLock lock = new ReentrantLock();
    private BackendFactory backendFactory;

    public BackendFactory getBackendFactory() {
        lock.lock();
        try {
            if(backendFactory != null) {
                return backendFactory;
            }

            String entrypointSourceCode =
                "export { \"" +
                    StringEscapeUtils.escapeEcmaScript(exportName) +
                    "\" as factory } from \"" +
                    StringEscapeUtils.escapeEcmaScript(importPath) +
                    "\";";

            var entrypointSource = Source.newBuilder("js", entrypointSourceCode, "entrypoint.js")
                .mimeType("application/javascript+module")
                .build();


            var jsBackendFactory = jsContext.eval(entrypointSource).getMember("factory");

            backendFactory = new WrappedJSApiBackend(jsBackendFactory);
            return backendFactory;
        }
        catch(IOException e) {
            throw new UncheckedIOException(e);
        }
        finally {
            lock.unlock();
        }
    }

    @Override
    public void close() throws Exception {
        jsContext.close();
    }

    private final class WrappedJSApiBackend implements BackendFactory {
        public WrappedJSApiBackend(Value backendFactory) {
            this.backendFactory = backendFactory;
        }

        private final Value backendFactory;

        @Override
        public <TE, EE extends Throwable> Backend<TE, ?> create(ErrorType<TE, EE> errorType, HostOperations<TE> hostOperations) throws InterruptedException {
            return jsExecutor.runOnJSThreadWithoutError(() -> {
                var errorChecker = ErrorTypeAdapter.toJS(jsContext, jsExecutor, errorType);
                var jsBackend = jsContext.eval("js", "(errorChecker, backendFactory) => backendFactory.create(errorChecker, {}, x => x)").execute(errorChecker, backendFactory);

                return Backend.jsAdapter(
                    JSAdapter.identity(),
                    errorType,
                    JSAdapter.VALUE_ADAPTER
                ).fromJS(jsContext, jsExecutor, jsBackend);
            }).get();
        }
    }
}
