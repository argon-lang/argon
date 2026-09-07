package dev.argon.externcompiler;

import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class MainTest {
    @TempDir Path temp;

    @Test void compilesAndRunsGenerators() throws Exception {
        Path sources = temp.resolve("sources");
        source(sources, "test/Generator.java", generator("test.Generated", "test.Second"));
        Result result = compile(sources);
        assertEquals(0, result.status, result.diagnostics);
        assertTrue(Files.isRegularFile(temp.resolve("classes/test/Generated.class")));
        assertTrue(Files.isRegularFile(temp.resolve("classes/test/Second.class")));
    }

    @Test void missingAndEmptySourcesAreSuccessfulNoOps() throws Exception {
        assertEquals(0, compile(temp.resolve("missing")).status);
        Files.createDirectories(temp.resolve("empty"));
        assertEquals(0, compile(temp.resolve("empty")).status);
        assertFalse(Files.exists(temp.resolve("classes")));
    }

    @Test void reportsInvalidJava() throws Exception {
        Path sources = temp.resolve("invalid");
        source(sources, "Broken.java", "this is not Java");
        Result result = compile(sources);
        assertNotEquals(0, result.status);
        assertTrue(result.diagnostics.contains("Broken.java"), result.diagnostics);
    }

    @Test void rejectsSourcesWithoutGenerators() throws Exception {
        Path sources = temp.resolve("none");
        source(sources, "test/Utility.java", "package test; public final class Utility {}");
        Result result = compile(sources);
        assertNotEquals(0, result.status);
        assertTrue(result.diagnostics.contains("No ExternGenerator"), result.diagnostics);
    }

    @Test void rejectsInvalidAndDuplicateClassfiles() throws Exception {
        Path invalid = temp.resolve("invalid-bytes");
        source(invalid, "test/Generator.java", """
                package test;
                import dev.argon.externcompiler.ExternGenerator;
                public final class Generator implements ExternGenerator {
                    public java.util.Collection<byte[]> generate() { return java.util.List.of(new byte[] { 1, 2, 3 }); }
                }
                """);
        assertNotEquals(0, compile(invalid).status);

        Path duplicate = temp.resolve("duplicates");
        source(duplicate, "test/Generator.java", generator("test.Same", "test.Same"));
        Result result = compile(duplicate);
        assertNotEquals(0, result.status);
        assertTrue(result.diagnostics.contains("Duplicate generated class"), result.diagnostics);
    }

    @Test void successfulGenerationRemovesStaleClasses() throws Exception {
        Path stale = temp.resolve("classes/old/Stale.class");
        Files.createDirectories(stale.getParent());
        Files.write(stale, new byte[] { 1 });
        Path sources = temp.resolve("fresh");
        source(sources, "test/Generator.java", generator("test.Fresh"));
        assertEquals(0, compile(sources).status);
        assertFalse(Files.exists(stale));
        assertTrue(Files.exists(temp.resolve("classes/test/Fresh.class")));
    }

    @Test void generatorFailurePreservesExistingOutput() throws Exception {
        Path existing = temp.resolve("classes/old/Existing.class");
        Files.createDirectories(existing.getParent());
        Files.write(existing, new byte[] { 1 });
        Path sources = temp.resolve("throwing");
        source(sources, "test/Generator.java", """
                package test;
                import dev.argon.externcompiler.ExternGenerator;
                public final class Generator implements ExternGenerator {
                    public java.util.Collection<byte[]> generate() { throw new IllegalStateException("generator broke"); }
                }
                """);
        Result result = compile(sources);
        assertNotEquals(0, result.status);
        assertTrue(result.diagnostics.contains("generator broke"), result.diagnostics);
        assertTrue(Files.exists(existing));
    }

    private Result compile(Path sources) throws Exception {
        if (Files.isDirectory(sources)
                && !Files.exists(sources.resolve("module-info.java"))
                && sourceTreeContainsJava(sources)) {
            String provider = Files.exists(sources.resolve("test/Generator.java"))
                    ? " provides dev.argon.externcompiler.ExternGenerator with test.Generator;"
                    : "";
            Files.writeString(sources.resolve("module-info.java"),
                    "module test.extern.generators { requires dev.argon.externcompiler; exports test;" + provider + " }");
        }
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        int status = Main.run(new String[] {
                "--sources", sources.toString(), "--output", temp.resolve("classes").toString()
        }, new PrintStream(diagnostics, true, StandardCharsets.UTF_8));
        return new Result(status, diagnostics.toString(StandardCharsets.UTF_8));
    }

    private static boolean sourceTreeContainsJava(Path sources) throws Exception {
        try (var paths = Files.walk(sources)) {
            return paths.anyMatch(path -> Files.isRegularFile(path)
                    && path.getFileName().toString().endsWith(".java"));
        }
    }

    private static void source(Path root, String relative, String contents) throws Exception {
        Path path = root.resolve(relative);
        Files.createDirectories(path.getParent());
        Files.writeString(path, contents);
    }

    private static String generator(String... names) {
        String values = java.util.Arrays.stream(names)
                .map(name -> "classFile.build(ClassDesc.of(\"" + name + "\"), cb -> cb.withFlags(AccessFlag.PUBLIC))")
                .collect(java.util.stream.Collectors.joining(", "));
        return """
                package test;
                import dev.argon.externcompiler.ExternGenerator;
                import java.lang.classfile.ClassFile;
                import java.lang.constant.ClassDesc;
                import java.lang.reflect.AccessFlag;
                public final class Generator implements ExternGenerator {
                    public java.util.Collection<byte[]> generate() {
                        var classFile = ClassFile.of();
                        return java.util.List.of(%s);
                    }
                }
                """.formatted(values);
    }

    private record Result(int status, String diagnostics) {}
}
