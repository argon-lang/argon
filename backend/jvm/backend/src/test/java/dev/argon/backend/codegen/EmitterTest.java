package dev.argon.backend.codegen;

import com.google.common.collect.ImmutableMap;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.Classfile;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.jvmbackendmetadata.ModuleMetadata;
import dev.argon.vm.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.tools.ToolProvider;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.AccessFlag;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.*;

final class EmitterTest {
	@TempDir
	private Path tempDir;

	@Test
	void emitClassFilesCopiesAdditionalClassfilesAndGeneratesModuleInfo() throws Exception {
		var classfiles = compileClassfiles(
			source(
				"test/pkg/Foo.java",
				"""
				package test.pkg;

				public final class Foo {
				}
				"""
			)
		);

		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(
				new ModulePath(List.of("Test", "Pkg")),
				Optional.of("test.pkg")
			))),
			Optional.of(classfiles)
		);

		var entries = emitEntries(new TestProgramModel(platformMetadata, List.of()));

		assertTrue(entries.containsKey("test/pkg/Foo.class"));
		var moduleInfo = Classfile.parse(entries.get("module-info.class")).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals("test.module", module.moduleName().name().stringValue());
		assertEquals(Set.of("test.pkg"), exportedPackages(module));
	}

	@Test
	void emitClassFilesAddsMissingExportsToExistingModuleInfo() throws Exception {
		var classfiles = compileClassfiles(
			source(
				"module-info.java",
				"""
				module test.module {
					exports existing.pkg;
				}
				"""
			),
			source(
				"existing/pkg/Existing.java",
				"""
				package existing.pkg;

				public final class Existing {
				}
				"""
			)
		);

		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(
				new ModulePath(List.of("Added", "Pkg")),
				Optional.of("added.pkg")
			))),
			Optional.of(classfiles)
		);

		var entries = emitEntries(new TestProgramModel(platformMetadata, List.of()));

		assertTrue(entries.containsKey("existing/pkg/Existing.class"));
		var moduleInfo = Classfile.parse(entries.get("module-info.class")).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals(Set.of("existing.pkg", "added.pkg"), exportedPackages(module));
	}

	@Test
	void emitRecordWritesRecordClass() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Records"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.records")))),
			Optional.empty()
		);
		var recordDefinition = new RecordDefinition(
			UnsignedBigInteger.ZERO,
			new ImportSpecifier.Global(
				UnsignedBigInteger.ZERO,
				new Identifier.Named("Person"),
				new ErasedSignature(List.of(), new ErasedSignatureType.String())
			),
			new FunctionSignature(
				List.of(),
				List.of(),
				new Token.Builtin(new BuiltinType.String())
			),
			List.of(
				new RecordFieldDefinition(
					new Identifier.Named("name"),
					new Token.Builtin(new BuiltinType.String()),
					false
				),
				new RecordFieldDefinition(
					new Identifier.Named("active"),
					new Token.Builtin(new BuiltinType.Bool()),
					true
				)
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(new ProgramModel.ModuleExportEntry.RecordDefinition(
				new TubeFileEntry.RecordDefinition(recordDefinition)
			))
		));

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules));

		var classBytes = entries.get("test/records/Person.class");
		assertNotNull(classBytes);
		var classModel = Classfile.parse(classBytes).model();
		assertEquals("test/records/Person", classModel.thisClass().asInternalName());

		var nameField = classModel.fields().stream()
			.filter(field -> field.fieldName().equalsString("name"))
			.findAny()
			.orElseThrow();
		assertTrue(nameField.flags().has(AccessFlag.PUBLIC));
		assertTrue(nameField.flags().has(AccessFlag.FINAL));
		assertEquals("Ljava/lang/String;", nameField.fieldType().stringValue());

		var activeField = classModel.fields().stream()
			.filter(field -> field.fieldName().equalsString("active"))
			.findAny()
			.orElseThrow();
		assertTrue(activeField.flags().has(AccessFlag.PUBLIC));
		assertFalse(activeField.flags().has(AccessFlag.FINAL));
		assertEquals("Z", activeField.fieldType().stringValue());

		var constructor = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString("<init>"))
			.findAny()
			.orElseThrow();
		assertEquals("(Ljava/lang/String;Z)V", constructor.methodType().stringValue());
	}

	private Map<String, byte[]> emitEntries(ProgramModel program) throws IOException {
		var jar = new ByteArrayOutputStream();
		try(var zip = new ZipOutputStream(jar)) {
			new Emitter(program, zip).emitClassFiles();
		}

		var entries = new HashMap<String, byte[]>();
		try(var zip = new ZipInputStream(new ByteArrayInputStream(jar.toByteArray()))) {
			for(var entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
				entries.put(entry.getName(), zip.readAllBytes());
			}
		}

		return entries;
	}

	private Set<String> exportedPackages(java.lang.classfile.attribute.ModuleAttribute module) {
		var packages = new HashSet<String>();
		for(var exportInfo : module.exports()) {
			packages.add(exportInfo.exportedPackage().asSymbol().name());
		}

		return packages;
	}

	private List<Classfile> compileClassfiles(SourceFile... sources) throws IOException {
		var sourceRoot = tempDir.resolve("src-" + System.nanoTime());
		var classes = tempDir.resolve("classes-" + System.nanoTime());
		Files.createDirectories(classes);

		var sourceFiles = new ArrayList<String>();
		for(var source : sources) {
			var path = sourceRoot.resolve(source.relativePath());
			Files.createDirectories(path.getParent());
			Files.writeString(path, source.contents(), StandardCharsets.UTF_8);
			sourceFiles.add(path.toString());
		}

		var compiler = ToolProvider.getSystemJavaCompiler();
		if(compiler == null) {
			throw new IllegalStateException("Tests must run on a JDK");
		}

		var args = new ArrayList<String>();
		args.add("-d");
		args.add(classes.toString());
		args.addAll(sourceFiles);

		var exitCode = compiler.run(null, null, null, args.toArray(String[]::new));
		if(exitCode != 0) {
			throw new IllegalStateException("javac failed with exit code " + exitCode);
		}

		try(var paths = Files.walk(classes)) {
			return paths
				.filter(Files::isRegularFile)
				.filter(path -> path.getFileName().toString().endsWith(".class"))
				.sorted()
				.map(path -> {
					try {
						return Classfile.parse(Files.readAllBytes(path));
					}
					catch(IOException ex) {
						throw new UncheckedIOException(ex);
					}
				})
				.toList();
		}
	}

	private static SourceFile source(String relativePath, String contents) {
		return new SourceFile(relativePath, contents);
	}

	private record SourceFile(String relativePath, String contents) {
	}

	private record TestProgramModel(
		JvmPlatformTubeMetadata platformMetadata,
		List<ProgramModel.ModuleModel> modules
	) implements ProgramModel {
		private static final TubeName TUBE_NAME = new TubeName("Test", List.of());

		@Override
		public TubeHeader header() {
			throw new UnsupportedOperationException();
		}

		@Override
		public TubeMetadata metadata() {
			throw new UnsupportedOperationException();
		}

		@Override
		public DecodedMetadata decodedMetadata() {
			return new DecodedMetadata(platformMetadata, ImmutableMap.of());
		}

		@Override
		public TubeInfo getTubeInfo(UnsignedBigInteger id) {
			assertEquals(UnsignedBigInteger.ZERO, id);
			return new TubeInfo(TUBE_NAME, platformMetadata);
		}

		@Override
		public ModuleInfo getModuleInfo(UnsignedBigInteger id) {
			assertEquals(UnsignedBigInteger.ZERO, id);
			return new ModuleInfo(UnsignedBigInteger.ZERO, modules.getFirst().path());
		}

		@Override
		public FunctionInfo getFunctionInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public RecordInfo getRecordInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public RecordFieldInfo getRecordFieldInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public EnumInfo getEnumInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public EnumVariantInfo getEnumVariantInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public TraitInfo getTraitInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public MethodInfo getMethodInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}

		@Override
		public InstanceInfo getInstanceInfo(UnsignedBigInteger id) {
			throw new UnsupportedOperationException();
		}
	}
}
