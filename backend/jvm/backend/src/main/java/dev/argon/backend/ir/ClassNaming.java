package dev.argon.backend.ir;

import dev.argon.backend.codegen.ProgramModel;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.Identifier;
import dev.argon.vm.ImportSpecifier;
import dev.argon.vm.ModulePath;
import dev.argon.vm.TubeName;
import dev.argon.vm.ErasedSignature;
import dev.argon.vm.ErasedSignatureType;

import java.lang.constant.ClassDesc;
import java.lang.constant.ModuleDesc;

public class ClassNaming {
	private ClassNaming() {}

	static ModuleDesc tubeModuleName(TubeName tubeName, JvmPlatformTubeMetadata platformMetadata) {
		return ModuleDesc.of(
			platformMetadata.moduleName()
				.orElseGet(() -> ClassNaming.defaultTubeModuleName(tubeName))
		);
	}

	static String tubeModulePackageName(ProgramModel program, ModulePath modulePath, UnsignedBigInteger tubeId) {
		var tubeInfo = program.getTubeInfo(tubeId);
		var metadata = tubeInfo.platformMetadata();
		var moduleMetadata = metadata.moduleMetadata()
			.flatMap(mapping ->
				mapping.stream()
					.filter(mm -> mm.modulePath().equals(modulePath))
					.flatMap(mm -> mm.packageName().stream())
					.findAny()
			);

		return moduleMetadata
			.orElseGet(() -> defaultModulePackageName(tubeInfo.tubeName(), modulePath));
	}

	static ClassDesc typeDefinitionClassDescriptor(ProgramModel program, ImportSpecifier importSpecifier) {
		return switch(importSpecifier) {
			case ImportSpecifier.Global global -> {
				var moduleInfo = program.getModuleInfo(global.moduleId());

				var globalName = identifierToName(global.name());
				if(
					globalName.startsWith("_") ||
						globalName.equals("package-info") ||
						globalName.equals("module-info") ||
						globalName.equals("Globals") ||
						globalName.equals("Main")
				) {
					globalName = "_" + globalName;
				}

				yield ClassDesc.of(
					moduleInfo.packageName().name(),
					globalName
				);
			}
			case ImportSpecifier.Local local -> {
				var parent = typeDefinitionClassDescriptor(program, local.parent());
				yield parent.nested("Nested" + local.index());
			}
		};
	}

	public static ClassDesc moduleGlobalFunctionsClassName(ProgramModel.ModuleInfo moduleInfo) {
		return ClassDesc.of(
			moduleInfo.packageName().name(),
			"Globals"
		);
	}

	public static String typeTokenParameterFieldName(int index) {
		return ":pt" + index;
	}

	public static String instanceParameterFieldName(int index) {
		return ":pv" + index;
	}

	static String functionName(ImportSpecifier importSpecifier) {
		return switch(importSpecifier) {
			case ImportSpecifier.Global global -> identifierToName(global.name()) + erasedSignatureSuffix(global.sig());
			case ImportSpecifier.Local local -> functionName(local.parent()) + ":k" + local.index();
		};
	}

	static String fieldName(Identifier identifier) {
		return identifierToName(identifier);
	}

	static String variantName(Identifier identifier) {
		return identifierToName(identifier);
	}

	static String methodName(Identifier identifier, ErasedSignature signature) {
		return identifierToName(identifier) + erasedSignatureSuffix(signature);
	}

	private static String erasedSignatureSuffix(ErasedSignature signature) {
		var builder = new StringBuilder();
		builder.append(":a");
		for(var parameter : signature.params()) {
			appendErasedSignatureType(builder, parameter);
		}
		builder.append(":r");
		appendErasedSignatureType(builder, signature.result());
		return builder.toString();
	}

	private static void appendErasedSignatureType(StringBuilder builder, ErasedSignatureType type) {
		switch(type) {
			case ErasedSignatureType.Int _ -> builder.append(":bint:a:e");
			case ErasedSignatureType.I8 _ -> builder.append(":bi8:a:e");
			case ErasedSignatureType.U8 _ -> builder.append(":bu8:a:e");
			case ErasedSignatureType.I16 _ -> builder.append(":bi16:a:e");
			case ErasedSignatureType.U16 _ -> builder.append(":bu16:a:e");
			case ErasedSignatureType.I32 _ -> builder.append(":bi32:a:e");
			case ErasedSignatureType.U32 _ -> builder.append(":bu32:a:e");
			case ErasedSignatureType.I64 _ -> builder.append(":bi64:a:e");
			case ErasedSignatureType.U64 _ -> builder.append(":bu64:a:e");
			case ErasedSignatureType.Bool _ -> builder.append(":bbool:a:e");
			case ErasedSignatureType.String _ -> builder.append(":bstring:a:e");
			case ErasedSignatureType.Never _ -> builder.append(":bnever:a:e");
			case ErasedSignatureType.Array array -> {
				builder.append(":barray:a");
				appendErasedSignatureType(builder, array.elementType());
				builder.append(":e");
			}
			case ErasedSignatureType.Function function -> {
				builder.append(":f");
				appendErasedSignatureType(builder, function.input());
				builder.append(":r");
				appendErasedSignatureType(builder, function.output());
				builder.append(":e");
			}
			case ErasedSignatureType.Record record -> {
				builder
					.append(":r")
					.append(importSpecifierSignatureName(record.recordImport()))
					.append(":a");
				for(var arg : record.args()) {
					appendErasedSignatureType(builder, arg);
				}
				builder.append(":e");
			}
			case ErasedSignatureType.Tuple tuple -> {
				builder.append(":t");
				for(var element : tuple.elements()) {
					appendErasedSignatureType(builder, element);
				}
				builder.append(":e");
			}
			case ErasedSignatureType.Erased _ -> builder.append(":_");
		}
	}

	private static String importSpecifierSignatureName(ImportSpecifier importSpecifier) {
		return switch(importSpecifier) {
			case ImportSpecifier.Global global ->
				identifierToName(global.name()) + erasedSignatureSuffix(global.sig());
			case ImportSpecifier.Local local ->
				importSpecifierSignatureName(local.parent()) + ":k" + local.index();
		};
	}

	static String defaultTubeModuleName(TubeName tubeName) {
		var name = new StringBuilder();
		appendDefaultTubeBasePackage(name, tubeName);
		return name.toString();
	}

	private static void appendDefaultTubeBasePackage(StringBuilder builder, TubeName tubeName) {
		builder
			.append("argontube")
			.append(tubeName.tail().size() + 1)
			.append('.');
		appendEncodedTubeName(builder, tubeName);
	}

	private static void appendEncodedTubeName(StringBuilder builder, TubeName tubeName) {
		appendEscapedIdentifier(builder, tubeName.head());

		for(var segment : tubeName.tail()) {
			builder.append('.');
			appendEscapedIdentifier(builder, segment);
		}
	}

	private static void appendDefaultModulePackageName(StringBuilder builder, TubeName tubeName, ModulePath modulePath) {
		appendDefaultTubeBasePackage(builder, tubeName);

		for(var segment : modulePath.path()) {
			builder.append('.');
			appendEscapedIdentifier(builder, segment);
		}
	}

	static String defaultModulePackageName(TubeName tubeName, ModulePath modulePath) {
		var name = new StringBuilder();
		appendDefaultModulePackageName(name, tubeName, modulePath);
		return name.toString();
	}

	private static void appendEscapedIdentifier(StringBuilder builder, String identifier) {
		if(identifier.isEmpty()) {
			builder.append("\\=");
			return;
		}

		var start = builder.length();
		var changed = false;

		for(var i = 0; i < identifier.length(); ++i) {
			var ch = identifier.charAt(i);
			var replacement = switch(ch) {
				case '/' -> '|';
				case '.' -> ',';
				case ';' -> '?';
				case '$' -> '%';
				case '<' -> '^';
				case '>' -> '_';
				case '[' -> '{';
				case ']' -> '}';
				case ':' -> '!';
				default -> 0;
			};

			if(replacement != 0) {
				builder.append('\\').append(replacement);
				changed = true;
			}
			else if(ch == '\\' && startsAccidentalEscape(identifier, i)) {
				builder.append("\\-");
				changed = true;
			}
			else {
				builder.append(ch);
			}
		}

		if(changed && builder.charAt(start) != '\\') {
			builder.insert(start, "\\=");
		}
	}

	private static String escapedIdentifier(String identifier) {
		var builder = new StringBuilder();
		appendEscapedIdentifier(builder, identifier);
		return builder.toString();
	}

	private static boolean startsAccidentalEscape(String identifier, int index) {
		if(index + 1 >= identifier.length()) {
			return false;
		}

		return switch(identifier.charAt(index + 1)) {
			case '|', ',', '?', '%', '^', '_', '{', '}', '!', '-' -> true;
			case '=' -> index == 0;
			default -> false;
		};
	}

	private static String identifierToName(Identifier identifier) {
		return switch(identifier) {
			case Identifier.BinOp(var binOp) -> ":b" + escapedIdentifier(binaryOperatorName(binOp));
			case Identifier.Extension(var inner) -> ":x" + identifierToName(inner);
			case Identifier.Index() -> ":n";
			case Identifier.Inverse(var inner) -> ":i" + identifierToName(inner);
			case Identifier.Named(var name) -> {
				var sb = new StringBuilder();
				appendEscapedIdentifier(sb, name);
				yield sb.toString();
			}
			case Identifier.UnOp(var unOp) -> ":u" + escapedIdentifier(unaryOperatorName(unOp));
			case Identifier.Update update -> ":m" + identifierToName(update.inner());
		};
	}

	private static String binaryOperatorName(dev.argon.vm.BinaryOperator operator) {
		return switch(operator) {
			case PLUS -> "+";
			case MINUS -> "-";
			case MUL -> "*";
			case DIV -> "/";
			case EQUAL -> "=";
			case NOT_EQUAL -> "!=";
			case LESS_THAN -> "<";
			case LESS_THAN_EQ -> "<=";
			case GREATER_THAN -> ">";
			case GREATER_THAN_EQ -> ">=";
			case BIT_OR -> "|||";
			case BIT_XOR -> "^^^";
			case BIT_AND -> "&&&";
			case SHIFT_LEFT -> "<<<";
			case SHIFT_RIGHT -> ">>>";
			case CONCAT -> "++";
		};
	}

	private static String unaryOperatorName(dev.argon.vm.UnaryOperator operator) {
		return switch(operator) {
			case PLUS -> "+";
			case MINUS -> "-";
			case BIT_NOT -> "~~~";
			case LOGICAL_NOT -> "!";
		};
	}



}
