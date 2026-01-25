package dev.argon.driver.launcher.backendloaders.jsApi;

import dev.argon.backend.api.Backend;
import dev.argon.backend.api.BackendFactory;
import dev.argon.backend.api.HostOperations;
import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.backend.api.metadata.JsLoaderOptions;
import dev.argon.nobleidl.runtime.ErrorType;
import dev.argon.nobleidl.runtime.graaljsInterop.ErrorTypeAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSExecutor;
import dev.argon.nobleidl.runtime.graaljsInterop.JSPromiseAdapter;
import org.apache.commons.text.StringEscapeUtils;
import org.graalvm.polyglot.*;
import org.graalvm.polyglot.io.IOAccess;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Map;

public class JSBackendFactory implements BackendFactory {
    public JSBackendFactory(
		Path backendDir,
		BackendMetadata metadata,
		JsLoaderOptions jsLoaderOptions
    ) {
		this.backendDir = backendDir;
		this.metadata = metadata;
		this.jsLoaderOptions = jsLoaderOptions;
        var executor = Executors.newSingleThreadExecutor();
        var javaExecutor = Executors.newVirtualThreadPerTaskExecutor();

        jsExecutor = JSExecutor.fromExecutors(executor, javaExecutor);
    }

	private final Path backendDir;
	private final BackendMetadata metadata;
	private final JsLoaderOptions jsLoaderOptions;
    private final JSExecutor jsExecutor;

    private final ReentrantLock lock = new ReentrantLock();
	private Context _jsContext;
    private Value _backendFactory;


	@Override
	public BackendMetadata metadata() {
		return metadata;
	}

	@Override
	public <TE, EE extends Throwable> Backend<TE, ?> create(ErrorType<TE, EE> errorType, HostOperations<TE> hostOperations) throws InterruptedException {
		return jsExecutor.runOnJSThreadWithoutError(() -> {
			var jsContext = getJSContext();
			var errorChecker = ErrorTypeAdapter.toJS(jsContext, jsExecutor, errorType);
			var jsBackend = jsContext.eval("js", "(errorChecker, backendFactory) => backendFactory.create(errorChecker, {}, x => x)").execute(errorChecker, getBackendFactory());

			return Backend.jsAdapter(
				JSAdapter.identity(),
				errorType,
				JSAdapter.VALUE_ADAPTER
			).fromJS(jsContext, jsExecutor, jsBackend);
		}).get();
	}

    @Override
    public void close() throws Exception {
	    var jsContext = _jsContext;
	    if(jsContext != null) {
		    jsContext.close();
	    }
    }

	private Context getJSContext() {
		lock.lock();
		try {
			if(_jsContext != null) {
				return _jsContext;
			}
			
			Map<String, String> files;
			try {
				files = loadDirectory(backendDir);
			}
			catch(IOException e) {
				throw new UncheckedIOException(e);
			}

			files = ImportResolver.resolveModules(jsLoaderOptions.importPath(), files);
			
			_jsContext = Context.newBuilder("js")
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
			
			return _jsContext;
		}
		finally {
			lock.unlock();
		}
	}

	private Value getBackendFactory() {
		lock.lock();
		try {
			if(_backendFactory != null) {
				return _backendFactory;
			}

			String entrypointSourceCode =
				"export { \"" +
					StringEscapeUtils.escapeJava(jsLoaderOptions.exportName()) +
					"\" as factory } from \"" +
					StringEscapeUtils.escapeJava(MemoryFileSystem.getFullPath(Path.of("/backend", jsLoaderOptions.importPath()))) +
					"\";";
			
			var entrypointSource = Source.newBuilder("js", entrypointSourceCode, "entrypoint.js")
				.mimeType("application/javascript+module")
				.build();


			_backendFactory = _jsContext.eval(entrypointSource).getMember("factory");
			return _backendFactory;
		}
		catch(IOException e) {
			throw new UncheckedIOException(e);
		}
		finally {
			lock.unlock();
		}
	}


	private static Map<String, String> loadDirectory(Path dir) throws IOException {
		var files = new HashMap<String, String>();

		try {
			try(var filesStream = Files.walk(dir)) {
				for(Iterator<@NotNull Path> it = filesStream.iterator(); it.hasNext(); ) {
					var path = it.next();

					if(!Files.isRegularFile(path)) {
						continue;
					}

					var name = path.getFileName().toString();
					if(!(name.endsWith(".js") || name.endsWith(".mjs") || name.equals("package.json"))) {
						continue;
					}

					var content = Files.readString(path);
					files.put("/backend/" + dir.relativize(path), content);
				}
			}
		}
		catch(UncheckedIOException e) {
			if(e.getCause() instanceof IOException ioe) {
				throw ioe;
			}
			throw e;
		}

		return files;
	}
}
