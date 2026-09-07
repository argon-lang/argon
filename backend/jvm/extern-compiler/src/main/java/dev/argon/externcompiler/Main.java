package dev.argon.externcompiler;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReader;
import java.lang.module.ModuleReference;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.stream.Stream;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;

public final class Main {
	private Main() {
	}

	public static void main(String[] args) {
		System.exit(run(args, System.err));
	}

	static int run(String[] args, PrintStream diagnostics) {
		Path sources = null;
		Path output = null;
		for(int i = 0; i < args.length; i += 2) {
			if(i + 1 >= args.length)
				return usage(diagnostics, "Missing value for " + args[i]);
			switch (args[i]) {
				case "--sources" -> sources = Path.of(args[i + 1]);
				case "--output" -> output = Path.of(args[i + 1]);
				default -> {
					return usage(diagnostics, "Unknown option: " + args[i]);
				}
			}
		}
		if(sources == null || output == null)
			return usage(diagnostics, "Both options are required");

		try {
			List<Path> sourceFiles = sourceFiles(sources);
			if(sourceFiles.isEmpty())
				return 0;
			Map<String, byte[]> compiledGenerators = compile(sourceFiles, diagnostics);
			if(compiledGenerators == null)
				return 1;
			Map<String, byte[]> classes = generate(compiledGenerators);
			if(classes.isEmpty())
				throw new IllegalArgumentException("No ExternGenerator implementations found");

			Path outputParent = output.toAbsolutePath().getParent();
			if(outputParent == null)
				throw new IllegalArgumentException("Output has no parent: " + output);
			Files.createDirectories(outputParent);
			Path staged = Files.createTempDirectory(outputParent, "extern-classes-");
			try {
				for(var entry : classes.entrySet()) {
					Path destination = staged.resolve(entry.getKey() + ".class");
					Files.createDirectories(destination.getParent());
					Files.write(destination, entry.getValue());
				}
				deleteTree(output);
				Files.createDirectories(output.getParent());
				Files.move(staged, output);
			}
			finally {
				deleteTree(staged);
			}
			return 0;
		}
		catch(Exception ex) {
			diagnostics.println("Extern generation failed: " + ex.getMessage());
			ex.printStackTrace(diagnostics);
			return 1;
		}
	}

	private static Map<String, byte[]> compile(List<Path> sources, PrintStream diagnostics) throws IOException {
		JavaCompiler compiler = javax.tools.ToolProvider.getSystemJavaCompiler();
		if(compiler == null)
			throw new IllegalStateException("No system Java compiler is available");
		Path modulePath;
		try {
			modulePath = Path.of(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		}
		catch(java.net.URISyntaxException ex) {
			throw new IllegalStateException("Could not locate extern-compiler module", ex);
		}
		List<String> options = List.of("--module-path", modulePath.toString(), "-implicit:none");
		try(StandardJavaFileManager standardManager = compiler.getStandardFileManager(null, null, null);
			MemoryFileManager manager = new MemoryFileManager(standardManager)) {
			var compilationUnits = standardManager.getJavaFileObjectsFromPaths(sources);
			boolean success = compiler
				.getTask(new java.io.PrintWriter(diagnostics, true), manager, null, options, null, compilationUnits)
				.call();
			return success ? manager.classBytes() : null;
		}
	}

	private static Map<String, byte[]> generate(Map<String, byte[]> compiledClasses) throws Exception {
		ModuleFinder generatorFinder = new MemoryModuleFinder(compiledClasses);
		var moduleReferences = generatorFinder.findAll();
		if(moduleReferences.size() != 1) {
			throw new IllegalArgumentException(
				"Expected exactly one extern generator module, found " + moduleReferences.size());
		}
		String moduleName = moduleReferences.iterator().next().descriptor().name();
		ModuleFinder finder = generatorFinder;
		if(ModuleLayer.boot().findModule("dev.argon.externcompiler").isEmpty()) {
			Path helperLocation = Path.of(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI());
			finder = ModuleFinder.compose(generatorFinder, ModuleFinder.of(helperLocation));
		}
		var configuration = ModuleLayer.boot().configuration().resolveAndBind(finder, ModuleFinder.of(),
			List.of(moduleName));
		Map<String, byte[]> layerClasses = compiledClasses;
		if(!Main.class.getModule().isNamed()) {
			var combined = new java.util.HashMap<>(compiledClasses);
			Path helperLocation = Path.of(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI());
			try(var paths = Files.walk(helperLocation)) {
				for(Path path : paths.filter(Files::isRegularFile)
					.filter(path -> path.getFileName().toString().endsWith(".class")).toList()) {
					String name = helperLocation.relativize(path).toString()
						.replace(path.getFileSystem().getSeparator(), ".").replaceFirst("\\.class$", "");
					combined.putIfAbsent(name, Files.readAllBytes(path));
				}
			}
			layerClasses = Map.copyOf(combined);
		}
		var loader = new MemoryClassLoader(layerClasses, ClassLoader.getSystemClassLoader());
		ModuleLayer layer = ModuleLayer.defineModules(configuration, List.of(ModuleLayer.boot()), _moduleName -> loader)
			.layer();
		Class<?> generatorType = layer.findLoader(moduleName).loadClass("dev.argon.externcompiler.ExternGenerator");
		List<?> generators;
		if(Main.class.getModule().isNamed()) {
			generators = ServiceLoader.load(layer, generatorType).stream()
				.sorted(Comparator.comparing(provider -> provider.type().getName())).map(ServiceLoader.Provider::get)
				.toList();
		}
		else {
			generators = moduleReferences.iterator().next().descriptor().provides().stream()
				.filter(provides -> provides.service().equals(ExternGenerator.class.getName()))
				.flatMap(provides -> provides.providers().stream()).sorted()
				.map(providerName -> instantiate(layer.findLoader(moduleName), providerName)).toList();
		}

		Map<String, byte[]> result = new java.util.TreeMap<>();
		for(Object generator : generators) {
			String generatorName = generator.getClass().getName();
			Object value = generatorType.getMethod("generate").invoke(generator);
			var generated = (java.util.Collection<?>) java.util.Objects.requireNonNull(value,
				"Generator returned null: " + generatorName);
			for(Object item : generated) {
				byte[] bytes = (byte[]) java.util.Objects.requireNonNull(item,
					"Generator returned a null classfile: " + generatorName);
				var model = java.lang.classfile.ClassFile.of().parse(bytes);
				String name = model.thisClass().asInternalName();
				if(name.startsWith("/") || name.contains("..") || name.contains("\\")) {
					throw new IllegalArgumentException("Unsafe generated class name: " + name);
				}
				if(result.putIfAbsent(name, bytes) != null) {
					throw new IllegalArgumentException("Duplicate generated class: " + name);
				}
			}
		}
		return result;
	}

	private static Object instantiate(ClassLoader loader, String className) {
		try {
			return loader.loadClass(className).getConstructor().newInstance();
		}
		catch(ReflectiveOperationException ex) {
			throw new IllegalArgumentException("Could not instantiate extern generator " + className, ex);
		}
	}

	private static final class MemoryFileManager extends ForwardingJavaFileManager<StandardJavaFileManager> {
		private final Map<String, MemoryClassFile> classes = new java.util.LinkedHashMap<>();

		private MemoryFileManager(StandardJavaFileManager fileManager) {
			super(fileManager);
		}

		@Override
		public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className,
			JavaFileObject.Kind kind, javax.tools.FileObject sibling) {
			var file = new MemoryClassFile(className, kind);
			classes.put(className, file);
			return file;
		}

		private Map<String, byte[]> classBytes() {
			var result = new java.util.LinkedHashMap<String, byte[]>();
			classes.forEach((name, file) -> result.put(name, file.bytes()));
			return Map.copyOf(result);
		}
	}

	private static final class MemoryClassFile extends SimpleJavaFileObject {
		private final ByteArrayOutputStream output = new ByteArrayOutputStream();

		private MemoryClassFile(String className, Kind kind) {
			super(URI.create("memory:///" + className.replace('.', '/') + kind.extension), kind);
		}

		@Override
		public java.io.OutputStream openOutputStream() {
			return output;
		}

		private byte[] bytes() {
			return output.toByteArray();
		}
	}

	private static final class MemoryModuleFinder implements ModuleFinder {
		private final ModuleReference module;

		private MemoryModuleFinder(Map<String, byte[]> classes) {
			byte[] moduleInfo = classes.get("module-info");
			if(moduleInfo == null)
				throw new IllegalArgumentException("Extern generators must declare a module");
			ModuleDescriptor descriptor = ModuleDescriptor.read(ByteBuffer.wrap(moduleInfo));
			module = new MemoryModuleReference(descriptor, classes);
		}

		@Override
		public Optional<ModuleReference> find(String name) {
			return module.descriptor().name().equals(name) ? Optional.of(module) : Optional.empty();
		}

		@Override
		public Set<ModuleReference> findAll() {
			return Set.of(module);
		}
	}

	private static final class MemoryModuleReference extends ModuleReference {
		private final Map<String, byte[]> resources;

		private MemoryModuleReference(ModuleDescriptor descriptor, Map<String, byte[]> classes) {
			super(descriptor, URI.create("memory:///" + descriptor.name()));
			var resources = new java.util.LinkedHashMap<String, byte[]>();
			classes.forEach((name, bytes) -> resources.put(name.replace('.', '/') + ".class", bytes));
			this.resources = Map.copyOf(resources);
		}

		@Override
		public ModuleReader open() {
			return new MemoryModuleReader(resources);
		}
	}

	private static final class MemoryModuleReader implements ModuleReader {
		private final Map<String, byte[]> resources;
		private MemoryModuleReader(Map<String, byte[]> resources) {
			this.resources = resources;
		}
		@Override
		public Optional<URI> find(String name) {
			return resources.containsKey(name) ? Optional.of(URI.create("memory:///" + name)) : Optional.empty();
		}
		@Override
		public Optional<InputStream> open(String name) {
			byte[] bytes = resources.get(name);
			return bytes == null ? Optional.empty() : Optional.of(new ByteArrayInputStream(bytes));
		}
		@Override
		public Optional<ByteBuffer> read(String name) {
			byte[] bytes = resources.get(name);
			return bytes == null ? Optional.empty() : Optional.of(ByteBuffer.wrap(bytes));
		}
		@Override
		public void release(ByteBuffer buffer) {
		}
		@Override
		public Stream<String> list() {
			return resources.keySet().stream();
		}
		@Override
		public void close() {
		}
	}

	private static final class MemoryClassLoader extends ClassLoader {
		private final Map<String, byte[]> classes;
		private MemoryClassLoader(Map<String, byte[]> classes, ClassLoader parent) {
			super(parent);
			this.classes = classes;
		}
		@Override
		protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
			synchronized (getClassLoadingLock(name)) {
				Class<?> type = findLoadedClass(name);
				if(type == null && classes.containsKey(name))
					type = findClass(name);
				if(type == null)
					type = super.loadClass(name, false);
				if(resolve)
					resolveClass(type);
				return type;
			}
		}
		@Override
		protected Class<?> findClass(String name) throws ClassNotFoundException {
			byte[] bytes = classes.get(name);
			if(bytes == null)
				throw new ClassNotFoundException(name);
			return defineClass(name, bytes, 0, bytes.length);
		}
		@Override
		protected Class<?> findClass(String moduleName, String name) {
			byte[] bytes = classes.get(name);
			return bytes == null ? null : defineClass(name, bytes, 0, bytes.length);
		}
	}

	private static List<Path> sourceFiles(Path directory) throws IOException {
		if(!Files.isDirectory(directory))
			return List.of();
		try(var paths = Files.walk(directory)) {
			return paths.filter(Files::isRegularFile).filter(path -> path.getFileName().toString().endsWith(".java"))
				.sorted(Comparator.comparing(Path::toString)).toList();
		}
	}

	private static void deleteTree(Path path) throws IOException {
		if(!Files.exists(path))
			return;
		Files.walkFileTree(path, new SimpleFileVisitor<>() {
			@Override
			public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) throws IOException {
				Files.delete(file);
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult postVisitDirectory(Path directory, IOException error) throws IOException {
				if(error != null)
					throw error;
				Files.delete(directory);
				return FileVisitResult.CONTINUE;
			}
		});
	}

	private static int usage(PrintStream diagnostics, String message) {
		diagnostics.println(message);
		diagnostics.println("Usage: extern-compiler --sources <dir> --output <dir>");
		return 2;
	}

}
