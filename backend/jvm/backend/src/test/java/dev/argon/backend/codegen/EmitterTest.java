package dev.argon.backend.codegen;

import com.google.common.collect.ImmutableMap;
import dev.argon.backend.ir.ClassNaming;
import dev.argon.backend.ir.ClassNamingProxy;
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
import java.lang.classfile.Opcode;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.FieldInstruction;
import java.lang.classfile.instruction.InvokeDynamicInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.classfile.instruction.TypeCheckInstruction;
import java.lang.constant.*;
import java.lang.reflect.AccessFlag;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.*;

final class EmitterTest {
	private static final JvmPlatformTubeMetadata EMPTY_PLATFORM_METADATA = new JvmPlatformTubeMetadata(
		Optional.empty(),
		Optional.empty(),
		Optional.empty()
	);

	@TempDir
	private Path tempDir;

	private static ProgramModel.FunctionInfo functionInfo(String packageName, FunctionDefinition function) {
		return new ProgramModel.FunctionInfo(
			ClassDesc.of(packageName, "Globals"),
			ClassNamingProxy.functionName(function._import()),
			function.signature(),
			functionDescriptor(function.signature())
		);
	}

	private static String functionName(FunctionDefinition function) {
		return ClassNamingProxy.functionName(function._import());
	}

	private static MethodTypeDesc functionDescriptor(FunctionSignature signature) {
		var parameterTypes = new ArrayList<ClassDesc>();
		for(var parameter : signature.tokenParameters()) {
			parameterTypes.add(tokenAsClassDesc(parameter.kind()));
		}
		for(var parameter : signature.parameters()) {
			parameterTypes.add(tokenAsClassDesc(parameter.paramType()));
		}

		return MethodTypeDesc.of(ClassDesc.of("dev.argon.runtime.Trampoline"), parameterTypes);
	}

	private static ClassDesc tokenAsClassDesc(Token token) {
		return switch(token) {
			case Token.Boxed _ -> ClassDesc.of("java.lang.Object");
			case Token.Builtin(var bt) -> switch(bt) {
				case BuiltinType.Array(var elementType) -> tokenAsClassDesc(elementType).arrayType();
				case BuiltinType.Bool() -> ClassDesc.ofDescriptor("Z");
				case BuiltinType.Conjunction _ -> throw new RuntimeException("Conjunction not implemented");
				case BuiltinType.Disjunction _ -> throw new RuntimeException("Disjunction not implemented");
				case BuiltinType.Int(var integerType) -> switch(integerType) {
					case INT -> ClassDesc.of("java.math.BigInteger");
					case I8 -> ClassDesc.ofDescriptor("I");
					case U8 -> ClassDesc.ofDescriptor("I");
					case I16 -> ClassDesc.ofDescriptor("I");
					case U16 -> ClassDesc.ofDescriptor("I");
					case I32 -> ClassDesc.ofDescriptor("I");
					case U32 -> ClassDesc.ofDescriptor("I");
					case I64 -> ClassDesc.ofDescriptor("J");
					case U64 -> ClassDesc.ofDescriptor("J");
				};
				case BuiltinType.Never() -> ClassDesc.of("dev.argon.runtime.Never");
				case BuiltinType.String() -> ClassDesc.of("java.lang.String");
			};
			case Token.Function _ -> ClassDesc.of("dev.argon.runtime.Function");
			case Token.FunctionErased _ -> ClassDesc.of("dev.argon.runtime.FunctionErased");
			case Token.FunctionToken _ -> ClassDesc.of("dev.argon.runtime.FunctionToken");
			case Token.ParentTokenParameter _, Token.TokenParameter _ -> ClassDesc.of("java.lang.Object");
			case Token.RefCell _ -> ClassDesc.of("dev.argon.runtime.RefCell");
			case Token.Tuple tuple -> {
				if(tuple.elements().size() > 10) {
					yield ClassDesc.of("dev.argon.runtime.TupleXL");
				}
				else {
					yield ClassDesc.of("dev.argon.runtime.Tuple" + tuple.elements().size());
				}
			}
			case Token.TypeInfo() -> ClassDesc.of("dev.argon.runtime.TypeInfo");
			case Token.Enum _, Token.InstanceType _, Token.InstanceValue _, Token.Record _, Token.Trait _ ->
				ClassDesc.of("java.lang.Object");
		};
	}

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

		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			List.of(new ProgramModel.ModuleModel(
				new ModulePath(List.of("Test", "Pkg")),
				List.of(),
				UnsignedBigInteger.ZERO
			))
		));

		assertTrue(entries.containsKey("test/pkg/Foo.class"));
		var moduleInfoBytes = entries.get("module-info.class");
		assertNotNull(moduleInfoBytes);
		var moduleInfo = Classfile.parse(moduleInfoBytes).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals("test.module", module.moduleName().name().stringValue());
		assertEquals(Set.of("test.pkg"), exportedPackages(module));
	}

	@Test
	void emitGeneratedModuleInfoRequiresReferencedTubes() throws Exception {
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.empty(),
			Optional.empty()
		);
		var referencedTube = new TubeName("Ref", List.of("Tube"));

		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			List.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(UnsignedBigInteger.ONE, new ProgramModel.TubeInfo(
				referencedTube,
				EMPTY_PLATFORM_METADATA,
				ModuleDesc.of("ref.tube.module")
			)),
			Map.of()
		));

		var moduleInfoBytes = entries.get("module-info.class");
		assertNotNull(moduleInfoBytes);
		var moduleInfo = Classfile.parse(moduleInfoBytes).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals(Set.of("java.base", "dev.argon.runtime", "ref.tube.module"), requiredModules(module));
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

		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			List.of(new ProgramModel.ModuleModel(
				new ModulePath(List.of("Added", "Pkg")),
				List.of(),
				UnsignedBigInteger.ZERO
			))
		));

		assertTrue(entries.containsKey("existing/pkg/Existing.class"));
		var moduleInfoBytes = entries.get("module-info.class");
		assertNotNull(moduleInfoBytes);
		var moduleInfo = Classfile.parse(moduleInfoBytes).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals(Set.of("existing.pkg", "added.pkg"), exportedPackages(module));
	}

	@Test
	void emitClassFilesAddsMissingRequiresToExistingModuleInfo() throws Exception {
		var classfiles = compileClassfiles(
			source(
				"module-info.java",
				"""
				module test.module {
					requires java.logging;
				}
				"""
			)
		);

		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.empty(),
			Optional.of(classfiles)
		);
		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			List.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(UnsignedBigInteger.ONE, new ProgramModel.TubeInfo(
				new TubeName("Added", List.of("Tube")),
				EMPTY_PLATFORM_METADATA,
				ModuleDesc.of("added.module")
			)),
			Map.of()
		));

		var moduleInfoBytes = entries.get("module-info.class");
		assertNotNull(moduleInfoBytes);
		var moduleInfo = Classfile.parse(moduleInfoBytes).model();
		var module = moduleInfo.findAttribute(java.lang.classfile.Attributes.module()).orElseThrow();
		assertEquals(Set.of("java.base", "dev.argon.runtime", "java.logging", "added.module"), requiredModules(module));
	}

	@Test
	void emitExecutableMainClass() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Executable"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.executable")))),
			Optional.empty()
		);
		var main = functionDefinition(
			0,
			"main",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), new Token.Tuple(List.of()))),
				new Token.Tuple(List.of())
			),
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.Return(new RegisterId(UnsignedBigInteger.ZERO))))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(main))),
			UnsignedBigInteger.ZERO
		));

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules), true);

		var mainClassBytes = entries.get("test/executable/Main.class");
		assertNotNull(mainClassBytes);
		var mainClass = Classfile.parse(mainClassBytes).model();
		var mainMethod = mainClass.methods().stream()
			.filter(method -> method.methodName().equalsString("main"))
			.findAny()
			.orElseThrow();
		assertTrue(mainMethod.flags().has(AccessFlag.PUBLIC));
		assertTrue(mainMethod.flags().has(AccessFlag.STATIC));
		assertEquals("([Ljava/lang/String;)V", mainMethod.methodType().stringValue());

		var invokeInstructions = mainMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
				invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("test/executable/Globals") &&
				invoke.name().equalsString(functionName(main)) &&
				invoke.type().equalsString("(Ldev/argon/runtime/Tuple0;)Ldev/argon/runtime/Trampoline;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Trampoline") &&
				invoke.name().equalsString("resolve") &&
				invoke.type().equalsString("(Ldev/argon/runtime/Trampoline;)Ljava/lang/Object;") &&
				invoke.isInterface()
		));
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
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(),
				new Token.Builtin(new BuiltinType.String())
			),
			List.of(
				new RecordFieldDefinition(
					UnsignedBigInteger.ZERO,
					new Identifier.Named("name"),
					new Token.Builtin(new BuiltinType.String()),
					false
				),
				new RecordFieldDefinition(
					UnsignedBigInteger.ONE,
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
			)),
			UnsignedBigInteger.ZERO
		));

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules));

		var classBytes = entries.get("test/records/Person.class");
		assertNotNull(classBytes);
		var classModel = Classfile.parse(classBytes).model();
		assertEquals("test/records/Person", classModel.thisClass().asInternalName());

		var tokenParameterField = classModel.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pt0"))
			.findAny()
			.orElseThrow();
		assertTrue(tokenParameterField.flags().has(AccessFlag.PUBLIC));
		assertTrue(tokenParameterField.flags().has(AccessFlag.FINAL));
		assertEquals("Ljava/lang/Object;", tokenParameterField.fieldType().stringValue());

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
		assertEquals("(Ltest/records/Person$Builder;)V", constructor.methodType().stringValue());
	}

	@Test
	void emitFunctionCallInstruction() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var signature = new FunctionSignature(
			List.of(),
			List.of(new SignatureParameter(Optional.empty(), intType)),
			intType
		);
		var parameterRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var resultRegister = new RegisterId(UnsignedBigInteger.ONE);
		var identity = functionDefinition(
			0,
			"identity",
			signature,
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.Return(parameterRegister)))
			)
		);
		var caller = functionDefinition(
			1,
			"caller",
			signature,
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(intType))),
				basicBlock(List.of(
					new Instruction.FunctionCall(
						UnsignedBigInteger.ZERO,
						new FunctionResult.Register(resultRegister),
						List.of(),
						List.of(parameterRegister)
					),
					new Instruction.Return(resultRegister)
				))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(identity)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(caller))
			),
			UnsignedBigInteger.ZERO
		));
		var functions = Map.of(
			UnsignedBigInteger.ZERO,
			functionInfo("test.functions", identity),
			UnsignedBigInteger.valueOf(1),
			functionInfo("test.functions", caller)
		);

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules, functions));

		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var classModel = Classfile.parse(globalsClass).model();
		var callerMethod = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		assertEquals("(Ljava/math/BigInteger;)Ldev/argon/runtime/Trampoline;", callerMethod.methodType().stringValue());
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("test/functions/Globals") &&
				invoke.name().equalsString(functionName(identity)) &&
				invoke.type().equalsString("(Ljava/math/BigInteger;)Ldev/argon/runtime/Trampoline;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Trampoline") &&
				invoke.name().equalsString("resolve") &&
				invoke.type().equalsString("(Ldev/argon/runtime/Trampoline;)Ljava/lang/Object;")
		));
	}

	@Test
	void emitFunctionCallReturnValueInstructionCreatesThunk() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var signature = new FunctionSignature(
			List.of(),
			List.of(new SignatureParameter(Optional.empty(), intType)),
			intType
		);
		var parameterRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var identity = functionDefinition(
			0,
			"identity",
			signature,
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.Return(parameterRegister)))
			)
		);
		var caller = functionDefinition(
			1,
			"caller",
			signature,
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.FunctionCall(
					UnsignedBigInteger.ZERO,
					new FunctionResult.ReturnValue(),
					List.of(),
					List.of(parameterRegister)
				)))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(identity)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(caller))
			),
			UnsignedBigInteger.ZERO
		));
		var functions = Map.of(
			UnsignedBigInteger.ZERO,
			functionInfo("test.functions", identity),
			UnsignedBigInteger.valueOf(1),
			functionInfo("test.functions", caller)
		);

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules, functions));

		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var classModel = Classfile.parse(globalsClass).model();
		var callerMethod = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		var dynamicInvoke = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeDynamicInstruction.class::isInstance)
			.map(InvokeDynamicInstruction.class::cast)
			.map(Objects::requireNonNull)
			.findAny()
			.orElseThrow();

		var trampoline = ClassDesc.of("dev.argon.runtime", "Trampoline");
		var thunk = ClassDesc.of("dev.argon.runtime", "Trampoline$Thunk");
		var functionType = MethodTypeDesc.of(trampoline, ClassDesc.of("java.math.BigInteger"));

		assertEquals("step", dynamicInvoke.name().stringValue());
		assertEquals(MethodTypeDesc.of(thunk, ClassDesc.of("java.math.BigInteger")), dynamicInvoke.typeSymbol());
		assertEquals(ClassDesc.of("java.lang.invoke.LambdaMetafactory"), dynamicInvoke.bootstrapMethod().owner());
		assertEquals("metafactory", dynamicInvoke.bootstrapMethod().methodName());
		assertEquals(List.of(
			MethodTypeDesc.of(trampoline),
			MethodHandleDesc.ofMethod(
				DirectMethodHandleDesc.Kind.STATIC,
				ClassDesc.of("test.functions", "Globals"),
				functionName(identity),
				functionType
			),
			MethodTypeDesc.of(trampoline)
		), dynamicInvoke.bootstrapArgs());

		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertFalse(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("test/functions/Globals") &&
			invoke.name().equalsString(functionName(identity))
		));
	}

	@Test
	void emitFunctionObjectCallInstruction() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var functionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var argumentRegister = new RegisterId(UnsignedBigInteger.ONE);
		var resultRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(
					new SignatureParameter(Optional.empty(), new Token.Function(intType, intType)),
					new SignatureParameter(Optional.empty(), intType)
				),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(intType))),
				basicBlock(List.of(
					new Instruction.FunctionObjectCall(
						new FunctionResult.Register(resultRegister),
						functionRegister,
						argumentRegister
					),
					new Instruction.Return(resultRegister)
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEINTERFACE &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Function") &&
				invoke.name().equalsString("apply") &&
				invoke.type().equalsString("(Ljava/lang/Object;)Ldev/argon/runtime/Trampoline;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Trampoline") &&
				invoke.name().equalsString("resolve")
		));
	}

	@Test
	void emitFunctionObjectTokenCallInstruction() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var functionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var resultRegister = new RegisterId(UnsignedBigInteger.ONE);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(new SignatureParameter(
					Optional.empty(),
					new Token.FunctionToken(new Token.TokenParameter(UnsignedBigInteger.ZERO), intType)
				)),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(intType))),
				basicBlock(List.of(
					new Instruction.FunctionObjectTokenCall(
						new FunctionResult.Register(resultRegister),
						functionRegister,
						new Token.TokenParameter(UnsignedBigInteger.ZERO)
					),
					new Instruction.Return(resultRegister)
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
				invoke.opcode() == Opcode.INVOKEINTERFACE &&
					invoke.owner().asInternalName().equals("dev/argon/runtime/FunctionToken") &&
					invoke.name().equalsString("apply") &&
					invoke.type().equalsString("(Ljava/lang/Object;)Ldev/argon/runtime/Trampoline;")
			));
	}

	@Test
	void emitFunctionObjectErasedCallReturnValueInstructionCreatesThunk() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var functionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), new Token.FunctionErased(intType))),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.FunctionObjectErasedCall(
					new FunctionResult.ReturnValue(),
					functionRegister
				)))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var dynamicInvoke = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeDynamicInstruction.class::isInstance)
			.map(InvokeDynamicInstruction.class::cast)
			.map(Objects::requireNonNull)
			.findAny()
			.orElseThrow();

		var trampoline = ClassDesc.of("dev.argon.runtime", "Trampoline");
		assertEquals("step", dynamicInvoke.name().stringValue());
		assertEquals(
			MethodTypeDesc.of(
				ClassDesc.of("dev.argon.runtime", "Trampoline$Thunk"),
				ClassDesc.of("dev.argon.runtime", "FunctionErased")
			),
			dynamicInvoke.typeSymbol()
		);
		assertEquals(List.of(
			MethodTypeDesc.of(trampoline),
			MethodHandleDesc.ofMethod(
				DirectMethodHandleDesc.Kind.INTERFACE_VIRTUAL,
				ClassDesc.of("dev.argon.runtime", "FunctionErased"),
				"apply",
				MethodTypeDesc.of(trampoline)
			),
			MethodTypeDesc.of(trampoline)
		), dynamicInvoke.bootstrapArgs());
	}

	@Test
	void emitPartiallyAppliedFunctionInstructions() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var concrete = functionDefinition(
			0,
			"concrete",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), intType)),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.Return(new RegisterId(UnsignedBigInteger.ZERO))))
			)
		);
		var tokenFunction = functionDefinition(
			1,
			"tokenFunction",
			new FunctionSignature(
				List.of(
					new SignatureTokenParameter(Optional.empty(), new Token.Boxed()),
					new SignatureTokenParameter(Optional.empty(), new Token.Boxed())
				),
				List.of(),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(intType))),
				basicBlock(List.of(
					new Instruction.ConstInt(new RegisterId(UnsignedBigInteger.ZERO), java.math.BigInteger.ONE),
					new Instruction.Return(new RegisterId(UnsignedBigInteger.ZERO))
				))
			)
		);
		var erased = functionDefinition(
			2,
			"erased",
			new FunctionSignature(
				List.of(),
				List.of(),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(intType))),
				basicBlock(List.of(
					new Instruction.ConstInt(new RegisterId(UnsignedBigInteger.ZERO), java.math.BigInteger.TWO),
					new Instruction.Return(new RegisterId(UnsignedBigInteger.ZERO))
				))
			)
		);
		var caller = functionDefinition(
			3,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(new Token.Function(intType, intType)),
					new VariableDeclaration(new Token.FunctionToken(new Token.Boxed(), intType)),
					new VariableDeclaration(new Token.FunctionErased(intType)),
					new VariableDeclaration(intType)
				)),
				basicBlock(List.of(
					new Instruction.PartiallyAppliedFunction(
						UnsignedBigInteger.ZERO,
						new RegisterId(UnsignedBigInteger.ZERO),
						List.of(),
						List.of()
					),
					new Instruction.PartiallyAppliedTokenFunction(
						UnsignedBigInteger.ONE,
						new RegisterId(UnsignedBigInteger.ONE),
						List.of(intType),
						List.of()
					),
					new Instruction.PartiallyAppliedFunctionErased(
						UnsignedBigInteger.valueOf(2),
						new RegisterId(UnsignedBigInteger.valueOf(2)),
						List.of(),
						List.of()
					),
					new Instruction.ConstInt(new RegisterId(UnsignedBigInteger.valueOf(3)), java.math.BigInteger.ZERO),
					new Instruction.Return(new RegisterId(UnsignedBigInteger.valueOf(3)))
				))
			)
		);
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(concrete)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(tokenFunction)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(erased)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(caller))
			),
			UnsignedBigInteger.ZERO
		));
		var functions = Map.of(
			UnsignedBigInteger.ZERO,
			functionInfo("test.functions", concrete),
			UnsignedBigInteger.ONE,
			functionInfo("test.functions", tokenFunction),
			UnsignedBigInteger.valueOf(2),
			functionInfo("test.functions", erased),
			UnsignedBigInteger.valueOf(3),
			functionInfo("test.functions", caller)
		);

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules, functions));
		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var callerMethod = Classfile.parse(globalsClass).model().methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		var dynamicInvokes = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeDynamicInstruction.class::isInstance)
			.map(InvokeDynamicInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertEquals(3, dynamicInvokes.size());
		assertTrue(dynamicInvokes.stream().allMatch(dynamicInvoke ->
			dynamicInvoke.name().equalsString("apply")
		));
		assertEquals(2, dynamicInvokes.stream()
			.filter(dynamicInvoke -> dynamicInvoke.bootstrapMethod().owner().equals(ClassDesc.of("java.lang.invoke.LambdaMetafactory")))
			.count());
		assertEquals(1, dynamicInvokes.stream()
			.filter(dynamicInvoke -> dynamicInvoke.bootstrapMethod().owner().equals(ClassDesc.of("dev.argon.runtime.PartialApplicationSupport")))
			.count());
		assertTrue(dynamicInvokes.stream().anyMatch(dynamicInvoke ->
			dynamicInvoke.typeSymbol().equals(MethodTypeDesc.of(ClassDesc.of("dev.argon.runtime", "Function")))
		));
		assertTrue(dynamicInvokes.stream().anyMatch(dynamicInvoke ->
			dynamicInvoke.typeSymbol().equals(MethodTypeDesc.of(
				ClassDesc.of("dev.argon.runtime", "FunctionToken"),
				ConstantDescs.CD_Object
			))
		));
		assertTrue(dynamicInvokes.stream().anyMatch(dynamicInvoke ->
			dynamicInvoke.typeSymbol().equals(MethodTypeDesc.of(ClassDesc.of("dev.argon.runtime", "FunctionErased")))
		));
	}

	@Test
	void emitIfElseInstruction() throws Exception {
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var conditionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var resultRegister = new RegisterId(UnsignedBigInteger.ONE);
		var whenTrueBlockId = new BlockId(UnsignedBigInteger.ZERO);
		var whenFalseBlockId = new BlockId(UnsignedBigInteger.ONE);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), boolType)),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(boolType))),
				new Region.Sequence(List.of(
					new Region.IfElse(
						whenTrueBlockId,
						whenFalseBlockId,
						basicBlock(List.of(new Instruction.BlockBreakUnless(whenFalseBlockId, conditionRegister))),
						basicBlock(List.of(new Instruction.ConstBool(resultRegister, true))),
						basicBlock(List.of(new Instruction.ConstBool(resultRegister, false)))
					),
					basicBlock(List.of(new Instruction.Return(resultRegister)))
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var branchInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(BranchInstruction.class::isInstance)
			.map(BranchInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(branchInstructions.stream().anyMatch(branch -> branch.opcode() == Opcode.IFEQ));
		assertTrue(branchInstructions.stream().anyMatch(branch -> branch.opcode() == Opcode.GOTO));
	}

	@Test
	void emitIfElseInstructionWithReturningBranches() throws Exception {
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var conditionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var whenTrueBlockId = new BlockId(UnsignedBigInteger.ZERO);
		var whenFalseBlockId = new BlockId(UnsignedBigInteger.ONE);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), boolType)),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(boolType),
					new VariableDeclaration(boolType)
				)),
				new Region.IfElse(
					whenTrueBlockId,
					whenFalseBlockId,
					basicBlock(List.of(new Instruction.BlockBreakUnless(whenFalseBlockId, conditionRegister))),
					basicBlock(List.of(
						new Instruction.ConstBool(new RegisterId(UnsignedBigInteger.ONE), true),
						new Instruction.Return(new RegisterId(UnsignedBigInteger.ONE))
					)),
					basicBlock(List.of(
						new Instruction.ConstBool(new RegisterId(UnsignedBigInteger.valueOf(2)), false),
						new Instruction.Return(new RegisterId(UnsignedBigInteger.valueOf(2)))
					))
				)
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var branchInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(BranchInstruction.class::isInstance)
			.map(BranchInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(branchInstructions.stream().anyMatch(branch -> branch.opcode() == Opcode.IFEQ));
		assertFalse(branchInstructions.stream().anyMatch(branch -> branch.opcode() == Opcode.GOTO));
	}

	@Test
	void emitReferenceInstructions() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var valueRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var refRegister = new RegisterId(UnsignedBigInteger.ONE);
		var loadedRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), intType)),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(new Token.RefCell(intType)),
					new VariableDeclaration(intType)
				)),
				basicBlock(List.of(
					new Instruction.NewReference(refRegister, valueRegister),
					new Instruction.LoadReference(loadedRegister, refRegister),
					new Instruction.UpdateReference(refRegister, valueRegister),
					new Instruction.Return(loadedRegister)
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESPECIAL &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/RefCell") &&
				invoke.name().equalsString("<init>") &&
				invoke.type().equalsString("(Ljava/lang/Object;)V")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/RefCell") &&
				invoke.name().equalsString("get") &&
				invoke.type().equalsString("()Ljava/lang/Object;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/RefCell") &&
				invoke.name().equalsString("set") &&
			invoke.type().equalsString("(Ljava/lang/Object;)V")
		));
	}

	@Test
	void emitUnboxInstruction() throws Exception {
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var boxedRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var resultRegister = new RegisterId(UnsignedBigInteger.ONE);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), new Token.Boxed())),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(new VariableDeclaration(boolType))),
				basicBlock(List.of(
					new Instruction.Unbox(resultRegister, boolType, boxedRegister),
					new Instruction.Return(resultRegister)
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("java/lang/Boolean") &&
				invoke.name().equalsString("booleanValue") &&
				invoke.type().equalsString("()Z")
		));
	}

	@Test
	void emitRaiseInstruction() throws Exception {
		var exceptionRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(new SignatureParameter(Optional.empty(), new Token.Boxed())),
				new Token.Builtin(new BuiltinType.Never())
			),
			new FunctionBody(
				new VariableDeclarations(List.of()),
				basicBlock(List.of(new Instruction.Raise(exceptionRegister)))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var typeChecks = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(TypeCheckInstruction.class::isInstance)
			.map(TypeCheckInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(typeChecks.stream().anyMatch(typeCheck ->
			typeCheck.opcode() == Opcode.CHECKCAST &&
				typeCheck.type().asInternalName().equals("dev/argon/runtime/ArgonException")
		));
	}

	@Test
	void emitRecordInstructions() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var recordType = new Token.Record(
			UnsignedBigInteger.ZERO,
			List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO))
		);
		var countRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var flagRegister = new RegisterId(UnsignedBigInteger.ONE);
		var recordRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var loadedRegister = new RegisterId(UnsignedBigInteger.valueOf(3));
		var recordImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("Pair"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var record = new RecordDefinition(
			UnsignedBigInteger.ZERO,
			recordImport,
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(),
				recordType
			),
			List.of(
				new RecordFieldDefinition(UnsignedBigInteger.ZERO, new Identifier.Named("flag"), boolType, true),
				new RecordFieldDefinition(UnsignedBigInteger.ONE, new Identifier.Named("count"), intType, true)
			)
		);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(
					new SignatureParameter(Optional.empty(), intType),
					new SignatureParameter(Optional.empty(), boolType)
				),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(recordType),
					new VariableDeclaration(boolType)
				)),
				basicBlock(List.of(
					new Instruction.RecordLiteral(
						recordRegister,
						recordType,
						List.of(
							new RecordFieldLiteral(UnsignedBigInteger.ZERO, flagRegister),
							new RecordFieldLiteral(UnsignedBigInteger.ONE, countRegister)
						)
					),
					new Instruction.RecordFieldLoad(UnsignedBigInteger.ZERO, loadedRegister, recordRegister),
					new Instruction.RecordFieldStore(UnsignedBigInteger.ZERO, recordRegister, flagRegister),
					new Instruction.Return(loadedRegister)
				))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.RecordDefinition(new TubeFileEntry.RecordDefinition(record)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(caller))
			),
			UnsignedBigInteger.ZERO
		));
		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			modules,
			Map.of(UnsignedBigInteger.ZERO, functionInfo("test.functions", caller)),
			Map.of(),
			Map.of()
		));

		assertNotNull(entries.get("test/functions/Pair.class"));
		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var callerMethod = Classfile.parse(globalsClass).model().methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("test/functions/Pair") &&
				invoke.name().equalsString("builder") &&
				invoke.type().equalsString("(Ljava/lang/Object;)Ltest/functions/Pair$Builder;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("test/functions/Pair$Builder") &&
				invoke.name().equalsString("set_flag") &&
				invoke.type().equalsString("(Z)Ltest/functions/Pair$Builder;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("test/functions/Pair$Builder") &&
				invoke.name().equalsString("set_count") &&
				invoke.type().equalsString("(Ljava/math/BigInteger;)Ltest/functions/Pair$Builder;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("test/functions/Pair$Builder") &&
				invoke.name().equalsString("build") &&
				invoke.type().equalsString("()Ltest/functions/Pair;")
		));

		var fieldInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(FieldInstruction.class::isInstance)
			.map(FieldInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(fieldInstructions.stream().anyMatch(field ->
			field.opcode() == Opcode.GETFIELD &&
				field.owner().asInternalName().equals("test/functions/Pair") &&
				field.name().equalsString("flag") &&
				field.type().equalsString("Z")
		));
		assertTrue(fieldInstructions.stream().anyMatch(field ->
			field.opcode() == Opcode.PUTFIELD &&
				field.owner().asInternalName().equals("test/functions/Pair") &&
				field.name().equalsString("flag") &&
				field.type().equalsString("Z")
		));
	}

	@Test
	void emitEnumDefinition() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var tokenParam = new Token.TokenParameter(UnsignedBigInteger.ZERO);
		var enumType = new Token.Enum(UnsignedBigInteger.ZERO, List.of(tokenParam));
		var enumImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("Choice"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var enumDef = new EnumDefinition(
			UnsignedBigInteger.ZERO,
			enumImport,
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(),
				enumType
			),
			List.of(new EnumVariantDefinition(
				UnsignedBigInteger.ZERO,
				new Identifier.Named("Some"),
				new FunctionSignature(
					List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
					List.of(new SignatureParameter(Optional.empty(), intType)),
					enumType
				),
				List.of(new RecordFieldDefinition(UnsignedBigInteger.ZERO, new Identifier.Named("active"), boolType, false))
			))
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(new ProgramModel.ModuleExportEntry.EnumDefinition(new TubeFileEntry.EnumDefinition(enumDef))),
			UnsignedBigInteger.ZERO
		));

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules));

		var enumClassBytes = entries.get("test/functions/Choice.class");
		assertNotNull(enumClassBytes);
		var enumClass = Classfile.parse(enumClassBytes).model();
		assertTrue(enumClass.flags().has(AccessFlag.PUBLIC));
		assertTrue(enumClass.flags().has(AccessFlag.ABSTRACT));
		var permittedSubclasses = enumClass.findAttribute(java.lang.classfile.Attributes.permittedSubclasses())
			.orElseThrow()
			.permittedSubclasses()
			.stream()
			.map(entry -> entry.asInternalName())
			.toList();
		assertEquals(List.of("test/functions/Choice$Some"), permittedSubclasses);

		var tokenParameterField = enumClass.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pt0"))
			.findAny()
			.orElseThrow();
		assertEquals("Ljava/lang/Object;", tokenParameterField.fieldType().stringValue());
		assertTrue(tokenParameterField.flags().has(AccessFlag.FINAL));

		var variantClassBytes = entries.get("test/functions/Choice$Some.class");
		assertNotNull(variantClassBytes);
		var variantClass = Classfile.parse(variantClassBytes).model();
		assertTrue(variantClass.flags().has(AccessFlag.PUBLIC));
		assertTrue(variantClass.flags().has(AccessFlag.FINAL));
		assertEquals("test/functions/Choice", variantClass.superclass().orElseThrow().asInternalName());

		var arg0Field = variantClass.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pv0"))
			.findAny()
			.orElseThrow();
		assertEquals("Ljava/lang/Object;", arg0Field.fieldType().stringValue());
		assertTrue(arg0Field.flags().has(AccessFlag.FINAL));

		var arg1Field = variantClass.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pv1"))
			.findAny()
			.orElseThrow();
		assertEquals("Ljava/math/BigInteger;", arg1Field.fieldType().stringValue());
		assertTrue(arg1Field.flags().has(AccessFlag.FINAL));

		var activeField = variantClass.fields().stream()
			.filter(field -> field.fieldName().equalsString("active"))
			.findAny()
			.orElseThrow();
		assertEquals("Z", activeField.fieldType().stringValue());
		assertTrue(activeField.flags().has(AccessFlag.FINAL));

		var constructor = variantClass.methods().stream()
			.filter(method -> method.methodName().equalsString("<init>"))
			.findAny()
			.orElseThrow();
		assertEquals(
			"(Ltest/functions/Choice$Some$Builder;)V",
			constructor.methodType().stringValue()
		);
	}

	@Test
	void emitIsEnumVariantOrBreakInstruction() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var enumType = new Token.Enum(
			UnsignedBigInteger.ZERO,
			List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO))
		);
		var enumImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("Choice"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var enumDef = new EnumDefinition(
			UnsignedBigInteger.ZERO,
			enumImport,
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(),
				enumType
			),
			List.of(new EnumVariantDefinition(
				UnsignedBigInteger.ZERO,
				new Identifier.Named("Some"),
				new FunctionSignature(
					List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
					List.of(new SignatureParameter(Optional.empty(), intType)),
					enumType
				),
				List.of(new RecordFieldDefinition(UnsignedBigInteger.ZERO, new Identifier.Named("active"), boolType, false))
			))
		);
		var countRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var activeRegister = new RegisterId(UnsignedBigInteger.ONE);
		var valueRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var argRegister = new RegisterId(UnsignedBigInteger.valueOf(3));
		var fieldRegister = new RegisterId(UnsignedBigInteger.valueOf(4));
		var tokenArgRegister = new RegisterId(UnsignedBigInteger.valueOf(5));
		var blockId = new BlockId(UnsignedBigInteger.ZERO);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(
					new SignatureParameter(Optional.empty(), intType),
					new SignatureParameter(Optional.empty(), boolType)
				),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(enumType),
					new VariableDeclaration(intType),
					new VariableDeclaration(boolType),
					new VariableDeclaration(new Token.Boxed())
				)),
				new Region.Sequence(List.of(
					basicBlock(List.of(
						new Instruction.EnumVariantLiteral(
							valueRegister,
							enumType,
							UnsignedBigInteger.ZERO,
							List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO)),
							List.of(countRegister),
							List.of(new RecordFieldLiteral(UnsignedBigInteger.ZERO, activeRegister))
						)
					)),
					new Region.Block(
						blockId,
						new BlockFlags(true, false, false),
						basicBlock(List.of(
							new Instruction.IsEnumVariantOrBreak(
								blockId,
								enumType,
								UnsignedBigInteger.ZERO,
								valueRegister,
								List.of(tokenArgRegister, argRegister),
								List.of(new FieldExtractor(fieldRegister, UnsignedBigInteger.ZERO))
							),
							new Instruction.Return(fieldRegister)
						))
					),
					basicBlock(List.of(
						new Instruction.ConstBool(fieldRegister, false),
						new Instruction.Return(fieldRegister)
					))
				))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.EnumDefinition(new TubeFileEntry.EnumDefinition(enumDef)),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(caller))
			),
			UnsignedBigInteger.ZERO
		));
		var functions = Map.of(
			UnsignedBigInteger.ZERO,
			functionInfo("test.functions", caller)
		);
		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			modules,
			functions,
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of()
		));

		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var callerMethod = Classfile.parse(globalsClass).model().methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		var typeChecks = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(TypeCheckInstruction.class::isInstance)
			.map(TypeCheckInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(typeChecks.stream().anyMatch(typeCheck ->
			typeCheck.opcode() == Opcode.INSTANCEOF &&
				typeCheck.type().asInternalName().equals("test/functions/Choice$Some")
		));
		assertTrue(typeChecks.stream().anyMatch(typeCheck ->
			typeCheck.opcode() == Opcode.CHECKCAST &&
				typeCheck.type().asInternalName().equals("test/functions/Choice$Some")
		));

		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESTATIC &&
				invoke.owner().asInternalName().equals("test/functions/Choice$Some") &&
				invoke.name().equalsString("builder") &&
				invoke.type().equalsString("(Ljava/lang/Object;Ljava/lang/Object;Ljava/math/BigInteger;)Ltest/functions/Choice$Some$Builder;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("test/functions/Choice$Some$Builder") &&
				invoke.name().equalsString("set_active") &&
				invoke.type().equalsString("(Z)Ltest/functions/Choice$Some$Builder;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("test/functions/Choice$Some$Builder") &&
				invoke.name().equalsString("build") &&
				invoke.type().equalsString("()Ltest/functions/Choice$Some;")
		));

		var fieldInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(FieldInstruction.class::isInstance)
			.map(FieldInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(fieldInstructions.stream().anyMatch(field ->
			field.opcode() == Opcode.GETFIELD &&
				field.owner().asInternalName().equals("test/functions/Choice$Some") &&
				field.name().equalsString(":pv1") &&
				field.type().equalsString("Ljava/math/BigInteger;")
		));
		assertTrue(fieldInstructions.stream().anyMatch(field ->
			field.opcode() == Opcode.GETFIELD &&
				field.owner().asInternalName().equals("test/functions/Choice$Some") &&
				field.name().equalsString("active") &&
				field.type().equalsString("Z")
		));
	}

	@Test
	void emitTraitDefinition() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Traits"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.traits")))),
			Optional.empty()
		);
		var traitImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("Comparable"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var compareSignature = new ErasedSignature(
			List.of(new ErasedSignatureType.Int()),
			new ErasedSignatureType.Bool()
		);
		var answerSignature = new ErasedSignature(
			List.of(),
			new ErasedSignatureType.Int()
		);
		var traitDefinition = new TraitDefinition(
			UnsignedBigInteger.ZERO,
			traitImport,
			new FunctionSignature(
				List.of(
					new SignatureTokenParameter(Optional.empty(), new Token.Boxed()),
					new SignatureTokenParameter(Optional.empty(), new Token.Boxed())
				),
				List.of(),
				new Token.Trait(UnsignedBigInteger.ZERO, List.of())
			),
			new Vtable(List.of()),
			List.of(
				new MethodDefinition(
					UnsignedBigInteger.ZERO,
					new Identifier.Named("compare"),
					compareSignature,
					new MethodFlags(true, false),
					new FunctionSignature(
						List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
						List.of(new SignatureParameter(Optional.empty(), new Token.Builtin(new BuiltinType.Int(IntegerType.INT)))),
						new Token.Builtin(new BuiltinType.Bool())
					),
					Optional.empty()
				),
				new MethodDefinition(
					UnsignedBigInteger.ONE,
					new Identifier.Named("answer"),
					answerSignature,
					new MethodFlags(false, false),
					new FunctionSignature(
						List.of(),
						List.of(new SignatureParameter(Optional.empty(), new Token.FunctionToken(
							new Token.Boxed(),
							new Token.Builtin(new BuiltinType.Int(IntegerType.INT))
						))),
						new Token.Builtin(new BuiltinType.Int(IntegerType.INT))
					),
					Optional.of(new FunctionImplementation.VmIr(new FunctionBody(
						new VariableDeclarations(List.of(new VariableDeclaration(new Token.Builtin(new BuiltinType.Int(IntegerType.INT))))),
						basicBlock(List.of(
							new Instruction.FunctionObjectTokenCall(
								new FunctionResult.Discard(),
								new RegisterId(UnsignedBigInteger.ONE),
								new Token.ParentTokenParameter(UnsignedBigInteger.ONE)
							),
							new Instruction.ConstInt(new RegisterId(UnsignedBigInteger.valueOf(2)), java.math.BigInteger.valueOf(42)),
							new Instruction.Return(new RegisterId(UnsignedBigInteger.valueOf(2)))
						))
					)))
				)
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(new ProgramModel.ModuleExportEntry.TraitDefinition(
				new TubeFileEntry.TraitDefinition(traitDefinition)
			)),
			UnsignedBigInteger.ZERO
		));

		var entries = emitEntries(new TestProgramModel(platformMetadata, modules));

		var classBytes = entries.get("test/traits/Comparable.class");
		assertNotNull(classBytes);
		var classModel = Classfile.parse(classBytes).model();
		assertTrue(classModel.flags().has(AccessFlag.PUBLIC));
		assertTrue(classModel.flags().has(AccessFlag.INTERFACE));
		assertTrue(classModel.flags().has(AccessFlag.ABSTRACT));

		var pt0 = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(":pt0"))
			.findAny()
			.orElseThrow();
		assertTrue(pt0.flags().has(AccessFlag.PUBLIC));
		assertTrue(pt0.flags().has(AccessFlag.ABSTRACT));
		assertEquals("()Ljava/lang/Object;", pt0.methodType().stringValue());

		var pt1 = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(":pt1"))
			.findAny()
			.orElseThrow();
		assertTrue(pt1.flags().has(AccessFlag.PUBLIC));
		assertTrue(pt1.flags().has(AccessFlag.ABSTRACT));
		assertEquals("()Ljava/lang/Object;", pt1.methodType().stringValue());

		var compare = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(
				ClassNamingProxy.methodName(new Identifier.Named("compare"), compareSignature)
			))
			.findAny()
			.orElseThrow();
		assertTrue(compare.flags().has(AccessFlag.PUBLIC));
		assertTrue(compare.flags().has(AccessFlag.ABSTRACT));
		assertEquals(
			"(Ljava/lang/Object;Ljava/math/BigInteger;)Ldev/argon/runtime/Trampoline;",
			compare.methodType().stringValue()
		);

		var answer = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(
				ClassNamingProxy.methodName(new Identifier.Named("answer"), answerSignature)
			))
			.findAny()
			.orElseThrow();
		assertTrue(answer.flags().has(AccessFlag.PUBLIC));
		assertFalse(answer.flags().has(AccessFlag.ABSTRACT));
		assertEquals("(Ldev/argon/runtime/FunctionToken;)Ldev/argon/runtime/Trampoline;", answer.methodType().stringValue());
		assertTrue(answer.code().isPresent());
		var answerInvokes = answer.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(answerInvokes.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEINTERFACE &&
					invoke.owner().asInternalName().equals("test/traits/Comparable") &&
					invoke.name().equalsString(":pt1") &&
					invoke.type().equalsString("()Ljava/lang/Object;")
		));
	}

	@Test
	void emitInstanceDefinition() throws Exception {
		var modulePath = new ModulePath(List.of("Test", "Instances"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.instances")))),
			Optional.empty()
		);
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var instanceImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("ShowInt"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var traitImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ZERO,
			new Identifier.Named("Show"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Int())
		);
		var coreExceptionTraitImport = new ImportSpecifier.Global(
			UnsignedBigInteger.ONE,
			new Identifier.Named("Exception"),
			new ErasedSignature(List.of(), new ErasedSignatureType.Erased())
		);
		var methodSignature = new ErasedSignature(
			List.of(),
			new ErasedSignatureType.Int()
		);
		var traitType = new Token.Trait(
			UnsignedBigInteger.ZERO,
			List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO))
		);
		var instanceDefinition = new InstanceDefinition(
			UnsignedBigInteger.ZERO,
			instanceImport,
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(new SignatureParameter(Optional.empty(), intType)),
				traitType
			),
			new Vtable(List.of()),
			List.of(new MethodDefinition(
				UnsignedBigInteger.ZERO,
				new Identifier.Named("show"),
				methodSignature,
				new MethodFlags(false, false),
				new FunctionSignature(
					List.of(),
					List.of(new SignatureParameter(Optional.empty(), new Token.FunctionToken(
						new Token.Boxed(),
						intType
					))),
					intType
				),
				Optional.of(new FunctionImplementation.VmIr(new FunctionBody(
					new VariableDeclarations(List.of(new VariableDeclaration(intType))),
					basicBlock(List.of(
						new Instruction.FunctionObjectTokenCall(
							new FunctionResult.Discard(),
							new RegisterId(UnsignedBigInteger.ONE),
							new Token.ParentTokenParameter(UnsignedBigInteger.ZERO)
						),
						new Instruction.ConstInt(new RegisterId(UnsignedBigInteger.valueOf(2)), java.math.BigInteger.valueOf(7)),
						new Instruction.Return(new RegisterId(UnsignedBigInteger.valueOf(2)))
					))
				)))
			))
		);
		var exceptionInstanceDefinition = new InstanceDefinition(
			UnsignedBigInteger.ONE,
			new ImportSpecifier.Global(
				UnsignedBigInteger.ZERO,
				new Identifier.Named("MyException"),
				new ErasedSignature(List.of(), new ErasedSignatureType.Erased())
			),
			new FunctionSignature(
				List.of(),
				List.of(),
				new Token.Trait(UnsignedBigInteger.ONE, List.of())
			),
			new Vtable(List.of()),
			List.of()
		);
		var receiverRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var functionRegister = new RegisterId(UnsignedBigInteger.ONE);
		var resultRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var instanceRegister = new RegisterId(UnsignedBigInteger.valueOf(3));
		var instanceFieldRegister = new RegisterId(UnsignedBigInteger.valueOf(4));
		var instanceType = new Token.InstanceType(
			UnsignedBigInteger.ZERO,
			List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO))
		);
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
				List.of(
					new SignatureParameter(Optional.empty(), traitType),
					new SignatureParameter(Optional.empty(), new Token.FunctionToken(new Token.Boxed(), intType))
				),
				intType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(intType),
					new VariableDeclaration(instanceType),
					new VariableDeclaration(intType)
				)),
				basicBlock(List.of(
					new Instruction.NewInstance(
						UnsignedBigInteger.ZERO,
						instanceRegister,
						List.of(new Token.TokenParameter(UnsignedBigInteger.ZERO)),
						List.of(resultRegister)
					),
					new Instruction.LoadInstanceField(
						instanceFieldRegister,
						instanceRegister,
						instanceType,
						UnsignedBigInteger.ZERO
					),
					new Instruction.InstanceMethodCall(
						new FunctionResult.Register(resultRegister),
						UnsignedBigInteger.ZERO,
						traitType,
						receiverRegister,
						List.of(),
						List.of(functionRegister)
					),
					new Instruction.Return(resultRegister)
				))
			)
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(
				new ProgramModel.ModuleExportEntry.InstanceDefinition(
					new TubeFileEntry.InstanceDefinition(instanceDefinition)
				),
				new ProgramModel.ModuleExportEntry.InstanceDefinition(
					new TubeFileEntry.InstanceDefinition(exceptionInstanceDefinition)
				),
				new ProgramModel.ModuleExportEntry.FunctionDefinition(
					new TubeFileEntry.FunctionDefinition(caller)
				)
			),
			UnsignedBigInteger.ZERO
		));

		var traits = Map.of(
			UnsignedBigInteger.ZERO,
			new ProgramModel.TraitInfo(
				traitImport,
				new FunctionSignature(
					List.of(new SignatureTokenParameter(Optional.empty(), new Token.Boxed())),
					List.of(),
					traitType
				),
				ClassDesc.of("test.instances", "Show")
			),
			UnsignedBigInteger.ONE,
			new ProgramModel.TraitInfo(
				coreExceptionTraitImport,
				new FunctionSignature(
					List.of(),
					List.of(),
					new Token.Trait(UnsignedBigInteger.ONE, List.of())
				),
				ClassDesc.of("argon.core.exception", "Exception")
			)
		);
		var instances = Map.of(
			UnsignedBigInteger.ZERO,
			new ProgramModel.InstanceInfo(
				instanceImport,
				instanceDefinition.signature(),
				ClassDesc.of("test.instances", "ShowInt"),
				MethodTypeDesc.of(
					ConstantDescs.CD_void,
					ClassDesc.of("java.lang.Object"),
					ClassDesc.of("java.math.BigInteger")
				)
			),
			UnsignedBigInteger.ONE,
			new ProgramModel.InstanceInfo(
				exceptionInstanceDefinition._import(),
				exceptionInstanceDefinition.signature(),
				ClassDesc.of("test.instances", "MyException"),
				MethodTypeDesc.of(ConstantDescs.CD_void)
			)
		);
		var methods = Map.of(
			UnsignedBigInteger.ZERO,
			new ProgramModel.MethodInfo(
				new Identifier.Named("show"),
				methodSignature,
				instanceDefinition.methods().getFirst().signature(),
				ClassDesc.of("test.instances", "Show"),
				ClassNamingProxy.methodName(new Identifier.Named("show"), methodSignature),
				functionDescriptor(instanceDefinition.methods().getFirst().signature())
			)
		);
		var entries = emitEntries(new TestProgramModel(
			platformMetadata,
			modules,
			Map.of(UnsignedBigInteger.ZERO, functionInfo("test.instances", caller)),
			Map.of(),
			Map.of(),
			Map.of(),
			Map.of(),
			traits,
			methods,
			instances,
			Map.of(
				UnsignedBigInteger.ONE,
				new ProgramModel.TubeInfo(
					new TubeName("Argon", List.of("Core")),
					new JvmPlatformTubeMetadata(
						Optional.empty(),
						Optional.of(List.of(new ModuleMetadata(
							new ModulePath(List.of("Exception")),
							Optional.of("argon.core.exception")
						))),
						Optional.empty()
					),
					ModuleDesc.of(ClassNamingProxy.defaultTubeModuleName(new TubeName("Argon", List.of("Core"))))
				)
			),
			Map.of(
				UnsignedBigInteger.ONE,
				new ProgramModel.ModuleInfo(
					UnsignedBigInteger.ONE,
					new ModulePath(List.of("Exception")),
					PackageDesc.of(ClassNamingProxy.defaultModulePackageName(
						new TubeName("Argon", List.of("Core")),
						new ModulePath(List.of("Exception"))
					))
				)
			)
		));

		var classBytes = entries.get("test/instances/ShowInt.class");
		assertNotNull(classBytes);
		var classModel = Classfile.parse(classBytes).model();
		assertTrue(classModel.flags().has(AccessFlag.PUBLIC));
		assertTrue(classModel.flags().has(AccessFlag.FINAL));
		assertEquals(
			List.of("test/instances/Show"),
			classModel.interfaces().stream().map(interfaceEntry -> interfaceEntry.asInternalName()).toList()
		);

		var exceptionClassBytes = entries.get("test/instances/MyException.class");
		assertNotNull(exceptionClassBytes);
		var exceptionClassModel = Classfile.parse(exceptionClassBytes).model();
		assertEquals(
			"dev/argon/runtime/ArgonException",
			exceptionClassModel.superclass().orElseThrow().asInternalName()
		);

		var tokenParameterField = classModel.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pt0"))
			.findAny()
			.orElseThrow();
		assertEquals("Ljava/lang/Object;", tokenParameterField.fieldType().stringValue());
		assertTrue(tokenParameterField.flags().has(AccessFlag.FINAL));

		var parameterField = classModel.fields().stream()
			.filter(field -> field.fieldName().equalsString(":pv0"))
			.findAny()
			.orElseThrow();
		assertEquals("Ljava/math/BigInteger;", parameterField.fieldType().stringValue());
		assertTrue(parameterField.flags().has(AccessFlag.FINAL));

		var constructor = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString("<init>"))
			.findAny()
			.orElseThrow();
		assertEquals("(Ljava/lang/Object;Ljava/math/BigInteger;)V", constructor.methodType().stringValue());

		var show = classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(
				ClassNamingProxy.methodName(new Identifier.Named("show"), methodSignature)
			))
			.findAny()
			.orElseThrow();
		assertEquals("(Ldev/argon/runtime/FunctionToken;)Ldev/argon/runtime/Trampoline;", show.methodType().stringValue());
		var fieldInstructions = show.code().orElseThrow()
			.elementStream()
			.filter(FieldInstruction.class::isInstance)
			.map(FieldInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(fieldInstructions.stream().anyMatch(field ->
					field.opcode() == Opcode.GETFIELD &&
					field.owner().asInternalName().equals("test/instances/ShowInt") &&
					field.name().equalsString(":pt0") &&
					field.type().equalsString("Ljava/lang/Object;")
		));

		var globalsClass = entries.get("test/instances/Globals.class");
		assertNotNull(globalsClass);
		var callerMethod = Classfile.parse(globalsClass).model().methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(caller)))
			.findAny()
			.orElseThrow();
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEINTERFACE &&
				invoke.owner().asInternalName().equals("test/instances/Show") &&
				invoke.name().equalsString(ClassNamingProxy.methodName(new Identifier.Named("show"), methodSignature)) &&
				invoke.type().equalsString("(Ldev/argon/runtime/FunctionToken;)Ldev/argon/runtime/Trampoline;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
				invoke.opcode() == Opcode.INVOKESPECIAL &&
				invoke.owner().asInternalName().equals("test/instances/ShowInt") &&
				invoke.name().equalsString("<init>") &&
				invoke.type().equalsString("(Ljava/lang/Object;Ljava/math/BigInteger;)V")
		));

		var callerFields = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(FieldInstruction.class::isInstance)
			.map(FieldInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();
		assertTrue(callerFields.stream().anyMatch(field ->
			field.opcode() == Opcode.GETFIELD &&
				field.owner().asInternalName().equals("test/instances/ShowInt") &&
				field.name().equalsString(":pv0") &&
				field.type().equalsString("Ljava/math/BigInteger;")
		));
	}

	@Test
	void emitTupleInstructions() throws Exception {
		var intType = new Token.Builtin(new BuiltinType.Int(IntegerType.INT));
		var boolType = new Token.Builtin(new BuiltinType.Bool());
		var tupleType = new Token.Tuple(List.of(intType, boolType));
		var countRegister = new RegisterId(UnsignedBigInteger.ZERO);
		var flagRegister = new RegisterId(UnsignedBigInteger.ONE);
		var tupleRegister = new RegisterId(UnsignedBigInteger.valueOf(2));
		var resultRegister = new RegisterId(UnsignedBigInteger.valueOf(3));
		var caller = functionDefinition(
			0,
			"caller",
			new FunctionSignature(
				List.of(),
				List.of(
					new SignatureParameter(Optional.empty(), intType),
					new SignatureParameter(Optional.empty(), boolType)
				),
				boolType
			),
			new FunctionBody(
				new VariableDeclarations(List.of(
					new VariableDeclaration(tupleType),
					new VariableDeclaration(boolType)
				)),
				basicBlock(List.of(
					new Instruction.Tuple(tupleRegister, List.of(countRegister, flagRegister)),
					new Instruction.TupleElement(UnsignedBigInteger.ONE, resultRegister, tupleRegister),
					new Instruction.Return(resultRegister)
				))
			)
		);

		var callerMethod = emittedSingleFunctionMethod(caller);
		var invokeInstructions = callerMethod.code().orElseThrow()
			.elementStream()
			.filter(InvokeInstruction.class::isInstance)
			.map(InvokeInstruction.class::cast)
			.map(Objects::requireNonNull)
			.toList();

		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKESPECIAL &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Tuple2") &&
				invoke.name().equalsString("<init>") &&
				invoke.type().equalsString("(Ljava/lang/Object;Ljava/lang/Object;)V")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("dev/argon/runtime/Tuple2") &&
				invoke.name().equalsString("element1") &&
				invoke.type().equalsString("()Ljava/lang/Object;")
		));
		assertTrue(invokeInstructions.stream().anyMatch(invoke ->
			invoke.opcode() == Opcode.INVOKEVIRTUAL &&
				invoke.owner().asInternalName().equals("java/lang/Boolean") &&
				invoke.name().equalsString("booleanValue") &&
				invoke.type().equalsString("()Z")
		));
	}

	private Map<String, byte[]> emitEntries(ProgramModel program) throws IOException {
		return emitEntries(program, false);
	}

	private Map<String, byte[]> emitEntries(ProgramModel program, boolean executable) throws IOException {
		var jar = new ByteArrayOutputStream();
		try(var zip = new ZipOutputStream(jar)) {
			var emitter = new Emitter(program, zip);
			if(executable) {
				emitter.markEmitExecutable();
			}
			emitter.emit();
		}

		var entries = new HashMap<String, byte[]>();
		try(var zip = new ZipInputStream(new ByteArrayInputStream(jar.toByteArray()))) {
			for(var entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
				entries.put(entry.getName(), zip.readAllBytes());
			}
		}

		return entries;
	}

	private java.lang.classfile.MethodModel emittedSingleFunctionMethod(FunctionDefinition function) throws IOException {
		var modulePath = new ModulePath(List.of("Test", "Functions"));
		var platformMetadata = new JvmPlatformTubeMetadata(
			Optional.of("test.module"),
			Optional.of(List.of(new ModuleMetadata(modulePath, Optional.of("test.functions")))),
			Optional.empty()
		);
		var modules = List.of(new ProgramModel.ModuleModel(
			modulePath,
			List.of(new ProgramModel.ModuleExportEntry.FunctionDefinition(new TubeFileEntry.FunctionDefinition(function))),
			UnsignedBigInteger.ZERO
		));
		var entries = emitEntries(new TestProgramModel(platformMetadata, modules));
		var globalsClass = entries.get("test/functions/Globals.class");
		assertNotNull(globalsClass);
		var classModel = Classfile.parse(globalsClass).model();

		return classModel.methods().stream()
			.filter(method -> method.methodName().equalsString(functionName(function)))
			.findAny()
			.orElseThrow();
	}

	private Set<String> exportedPackages(java.lang.classfile.attribute.ModuleAttribute module) {
		var packages = new HashSet<String>();
		for(var exportInfo : module.exports()) {
			packages.add(exportInfo.exportedPackage().asSymbol().name());
		}

		return packages;
	}

	private Set<String> requiredModules(java.lang.classfile.attribute.ModuleAttribute module) {
		var modules = new HashSet<String>();
		for(var requireInfo : module.requires()) {
			modules.add(requireInfo.requires().asSymbol().name());
		}

		return modules;
	}

	private List<Classfile> compileClassfiles(SourceFile... sources) throws IOException {
		var sourceRoot = tempDir.resolve("src-" + System.nanoTime());
		var classes = tempDir.resolve("classes-" + System.nanoTime());
		Files.createDirectories(classes);

		var sourceFiles = new ArrayList<String>();
		for(var source : sources) {
			var path = sourceRoot.resolve(source.relativePath());
			Files.createDirectories(Objects.requireNonNull(path.getParent()));
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
				.filter(path -> Objects.requireNonNull(path.getFileName()).toString().endsWith(".class"))
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

	private static FunctionDefinition functionDefinition(
		int id,
		String name,
		FunctionSignature signature,
		FunctionBody body
	) {
		return new FunctionDefinition(
			UnsignedBigInteger.valueOf(id),
			new ImportSpecifier.Global(
				UnsignedBigInteger.ZERO,
				new Identifier.Named(name),
				new ErasedSignature(List.of(), new ErasedSignatureType.Int())
			),
			new FunctionFlags(false),
			signature,
			Optional.of(new FunctionImplementation.VmIr(body))
		);
	}

	private static Region basicBlock(List<Instruction> instructions) {
		return new Region.BasicBlock(instructions);
	}

	private record SourceFile(String relativePath, String contents) {
	}

	private record TestProgramModel(
		JvmPlatformTubeMetadata platformMetadata,
		List<ProgramModel.ModuleModel> modules,
		Map<UnsignedBigInteger, FunctionInfo> functions,
		Map<UnsignedBigInteger, RecordInfo> records,
		Map<UnsignedBigInteger, RecordFieldInfo> recordFields,
		Map<UnsignedBigInteger, EnumInfo> enums,
		Map<UnsignedBigInteger, EnumVariantInfo> enumVariants,
		Map<UnsignedBigInteger, TraitInfo> traits,
		Map<UnsignedBigInteger, MethodInfo> methods,
		Map<UnsignedBigInteger, InstanceInfo> instances,
		Map<UnsignedBigInteger, TubeInfo> tubeInfos,
		Map<UnsignedBigInteger, ModuleInfo> moduleInfos
	) implements ProgramModel {
		private static final TubeName TUBE_NAME = new TubeName("Test", List.of());

		private TestProgramModel(
			JvmPlatformTubeMetadata platformMetadata,
			List<ProgramModel.ModuleModel> modules,
			Map<UnsignedBigInteger, FunctionInfo> functions,
			Map<UnsignedBigInteger, RecordInfo> records,
			Map<UnsignedBigInteger, RecordFieldInfo> recordFields,
			Map<UnsignedBigInteger, EnumInfo> enums,
			Map<UnsignedBigInteger, EnumVariantInfo> enumVariants,
			Map<UnsignedBigInteger, TraitInfo> traits,
			Map<UnsignedBigInteger, MethodInfo> methods,
			Map<UnsignedBigInteger, InstanceInfo> instances
		) {
			this(
				platformMetadata,
				modules,
				functions,
				records,
				recordFields,
				enums,
				enumVariants,
				traits,
				methods,
				instances,
				Map.of(),
				Map.of()
			);
		}

		private TestProgramModel(
			JvmPlatformTubeMetadata platformMetadata,
			List<ProgramModel.ModuleModel> modules,
			Map<UnsignedBigInteger, FunctionInfo> functions
		) {
			this(platformMetadata, modules, functions, Map.of(), Map.of(), Map.of(), Map.of(), Map.of(), Map.of(), Map.of());
		}

		private TestProgramModel(
			JvmPlatformTubeMetadata platformMetadata,
			List<ProgramModel.ModuleModel> modules,
			Map<UnsignedBigInteger, FunctionInfo> functions,
			Map<UnsignedBigInteger, RecordInfo> records,
			Map<UnsignedBigInteger, RecordFieldInfo> recordFields
		) {
			this(platformMetadata, modules, functions, records, recordFields, Map.of(), Map.of(), Map.of(), Map.of(), Map.of());
		}

		private TestProgramModel(
			JvmPlatformTubeMetadata platformMetadata,
			List<ProgramModel.ModuleModel> modules
		) {
			this(platformMetadata, modules, Map.of(), Map.of(), Map.of(), Map.of(), Map.of(), Map.of(), Map.of(), Map.of());
		}

		@Override
		public TubeHeader header() {
			throw new UnsupportedOperationException();
		}

		@Override
		public TubeMetadata metadata() {
			var referencedTubes = tubeInfos.entrySet().stream()
				.filter(entry -> entry.getKey().toBigInteger().signum() > 0)
				.sorted(Comparator.comparing(entry -> entry.getKey().toBigInteger()))
				.map(entry -> new TubeReference(entry.getValue().tubeName(), Optional.empty()))
				.toList();
			var moduleEntries = modules.stream()
				.map(module -> new dev.argon.vm.Module(module.path()))
				.toList();

			return new TubeMetadata(TUBE_NAME, referencedTubes, Optional.empty(), moduleEntries);
		}

		@Override
		public DecodedMetadata decodedMetadata() {
			return new DecodedMetadata(platformMetadata, ImmutableMap.of());
		}

		@Override
		public TubeInfo getTubeInfo(UnsignedBigInteger id) {
			var tubeInfo = tubeInfos.get(id);
			if(tubeInfo != null) {
				return tubeInfo;
			}

			assertEquals(UnsignedBigInteger.ZERO, id);
			return new TubeInfo(
				TUBE_NAME,
				platformMetadata,
				ModuleDesc.of(platformMetadata.moduleName().orElseGet(() -> ClassNamingProxy.defaultTubeModuleName(TUBE_NAME)))
			);
		}

		@Override
		public ModuleInfo getModuleInfo(UnsignedBigInteger id) {
			var moduleInfo = moduleInfos.get(id);
			if(moduleInfo != null) {
				return moduleInfo;
			}

			assertEquals(UnsignedBigInteger.ZERO, id);
				return new ModuleInfo(
					UnsignedBigInteger.ZERO,
					modules.getFirst().path(),
					PackageDesc.of(ClassNamingProxy.tubeModulePackageName(this, modules.getFirst().path(), UnsignedBigInteger.ZERO))
				);
			}

		@Override
		public FunctionInfo getFunctionInfo(UnsignedBigInteger id) {
			var function = functions.get(id);
			if(function != null) {
				return function;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.FunctionDefinition(var entry)) {
						var definition = entry.definition();
						if(definition.functionId().equals(id)) {
							return new FunctionInfo(
								ClassNaming.moduleGlobalFunctionsClassName(getModuleInfo(module.moduleId())),
								ClassNamingProxy.functionName(definition._import()),
								definition.signature(),
								functionDescriptor(definition.signature())
							);
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public RecordInfo getRecordInfo(UnsignedBigInteger id) {
			var record = records.get(id);
			if(record != null) {
				return record;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.RecordDefinition(var entry)) {
						var definition = entry.definition();
						if(definition.recordId().equals(id)) {
							var classDesc = typeDefinitionClassDesc(getModuleInfo(module.moduleId()).packageName(), definition._import());
							var builderClassDesc = classDesc.nested("Builder");
							return new RecordInfo(
								definition._import(),
								definition.signature(),
								classDesc,
								new RecordBuilderInfo(
									builderClassDesc,
									"builder",
									MethodTypeDesc.of(builderClassDesc, tokenParameterDescs(definition.signature())),
									"build",
									MethodTypeDesc.of(classDesc)
								)
							);
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public RecordFieldInfo getRecordFieldInfo(UnsignedBigInteger id) {
			var recordField = recordFields.get(id);
			if(recordField != null) {
				return recordField;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					switch(export) {
						case ProgramModel.ModuleExportEntry.RecordDefinition(var entry) -> {
							var definition = entry.definition();
							for(var field : definition.fields()) {
								if(field.fieldId().equals(id)) {
									return recordFieldInfo(
										RecordFieldInfo.OwnerType.RECORD,
										definition.recordId(),
										getRecordInfo(definition.recordId()).builderInfo().builderClassDesc(),
										field
									);
								}
							}
						}
						case ProgramModel.ModuleExportEntry.EnumDefinition(var entry) -> {
							for(var variant : entry.definition().variants()) {
								for(var field : variant.fields()) {
									if(field.fieldId().equals(id)) {
										return recordFieldInfo(
											RecordFieldInfo.OwnerType.ENUM_VARIANT,
											variant.variantId(),
											getEnumVariantInfo(variant.variantId()).builder().builderClassDesc(),
											field
										);
									}
								}
							}
						}
						default -> {}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public EnumInfo getEnumInfo(UnsignedBigInteger id) {
			var enumInfo = enums.get(id);
			if(enumInfo != null) {
				return enumInfo;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.EnumDefinition(var entry)) {
						var definition = entry.definition();
						if(definition.enumId().equals(id)) {
							return new EnumInfo(
								definition._import(),
								typeDefinitionClassDesc(getModuleInfo(module.moduleId()).packageName(), definition._import())
							);
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public EnumVariantInfo getEnumVariantInfo(UnsignedBigInteger id) {
			var enumVariant = enumVariants.get(id);
			if(enumVariant != null) {
				return enumVariant;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.EnumDefinition(var entry)) {
						var definition = entry.definition();
						var enumInfo = getEnumInfo(definition.enumId());
						for(var variant : definition.variants()) {
							if(variant.variantId().equals(id)) {
								var variantName = identifierName(variant.name());
								var variantClassDesc = enumInfo.enumClassDesc().nested(variantName);
								var builderClassDesc = variantClassDesc.nested("Builder");
								var parameterDescs = new ArrayList<ClassDesc>();
								parameterDescs.addAll(Arrays.asList(tokenParameterDescs(definition.signature())));
								parameterDescs.addAll(Arrays.asList(tokenParameterDescs(variant.signature())));
								for(var parameter : variant.signature().parameters()) {
									parameterDescs.add(tokenAsClassDesc(parameter.paramType()));
								}

								return new EnumVariantInfo(
									definition.enumId(),
									variant.name(),
									variant.signature(),
									variantName,
									variantClassDesc,
									new RecordBuilderInfo(
										builderClassDesc,
										"builder",
										MethodTypeDesc.of(builderClassDesc, parameterDescs),
										"build",
										MethodTypeDesc.of(variantClassDesc)
									)
								);
							}
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		private static RecordFieldInfo recordFieldInfo(
			RecordFieldInfo.OwnerType ownerType,
			UnsignedBigInteger ownerId,
			ClassDesc builderClassDesc,
			RecordFieldDefinition field
		) {
			var fieldName = identifierName(field.name());
			var fieldType = tokenAsClassDesc(field.fieldType());
			return new RecordFieldInfo(
				ownerType,
				ownerId,
				field.name(),
				fieldName,
				fieldType,
				"set_" + fieldName,
				MethodTypeDesc.of(builderClassDesc, fieldType)
			);
		}

		private static ClassDesc typeDefinitionClassDesc(PackageDesc packageName, ImportSpecifier importSpecifier) {
			return switch(importSpecifier) {
				case ImportSpecifier.Global global -> ClassDesc.of(packageName.name(), identifierName(global.name()));
				case ImportSpecifier.Local local -> typeDefinitionClassDesc(packageName, local.parent()).nested("Nested" + local.index());
			};
		}

		private static String identifierName(Identifier identifier) {
			return ClassNamingProxy.fieldName(identifier);
		}

		private static ClassDesc[] tokenParameterDescs(FunctionSignature signature) {
			return signature.tokenParameters().stream()
				.map(parameter -> tokenAsClassDesc(parameter.kind()))
				.toArray(ClassDesc[]::new);
		}

		@Override
		public TraitInfo getTraitInfo(UnsignedBigInteger id) {
			var trait = traits.get(id);
			if(trait != null) {
				return trait;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.TraitDefinition(var entry)) {
						var definition = entry.definition();
						if(definition.traitId().equals(id)) {
							return new TraitInfo(
								definition._import(),
								definition.signature(),
								typeDefinitionClassDesc(getModuleInfo(module.moduleId()).packageName(), definition._import())
							);
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public MethodInfo getMethodInfo(UnsignedBigInteger id) {
			var method = methods.get(id);
			if(method != null) {
				return method;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					switch(export) {
						case ProgramModel.ModuleExportEntry.TraitDefinition(var entry) -> {
							var traitInfo = getTraitInfo(entry.definition().traitId());
							for(var definition : entry.definition().methods()) {
								if(definition.methodId().equals(id)) {
									return new MethodInfo(
										definition.name(),
										definition.erasedSignature(),
										definition.signature(),
										traitInfo.traitDesc(),
										ClassNamingProxy.methodName(definition.name(), definition.erasedSignature()),
										functionDescriptor(definition.signature())
									);
								}
							}
						}
						case ProgramModel.ModuleExportEntry.InstanceDefinition(var entry) -> {
							var instanceInfo = getInstanceInfo(entry.definition().instanceId());
							for(var definition : entry.definition().methods()) {
								if(definition.methodId().equals(id)) {
									return new MethodInfo(
										definition.name(),
										definition.erasedSignature(),
										definition.signature(),
										instanceInfo.instanceClassDesc(),
										ClassNamingProxy.methodName(definition.name(), definition.erasedSignature()),
										functionDescriptor(definition.signature())
									);
								}
							}
						}
						default -> {}
					}
				}
			}

			throw new UnsupportedOperationException();
		}

		@Override
		public InstanceInfo getInstanceInfo(UnsignedBigInteger id) {
			var instance = instances.get(id);
			if(instance != null) {
				return instance;
			}

			for(var module : modules) {
				for(var export : module.exports()) {
					if(export instanceof ProgramModel.ModuleExportEntry.InstanceDefinition(var entry)) {
						var definition = entry.definition();
						if(definition.instanceId().equals(id)) {
							var parameterDescs = new ArrayList<ClassDesc>();
							parameterDescs.addAll(Arrays.asList(tokenParameterDescs(definition.signature())));
							for(var parameter : definition.signature().parameters()) {
								parameterDescs.add(tokenAsClassDesc(parameter.paramType()));
							}

							return new InstanceInfo(
								definition._import(),
								definition.signature(),
								typeDefinitionClassDesc(getModuleInfo(module.moduleId()).packageName(), definition._import()),
								MethodTypeDesc.of(ConstantDescs.CD_void, parameterDescs)
							);
						}
					}
				}
			}

			throw new UnsupportedOperationException();
		}
	}
}
