package dev.argon.argonvm.parser;

import com.google.protobuf.ByteString;
import com.google.protobuf.Empty;
import dev.argon.argonvm.Instruction;
import dev.argon.argonvm.NativeFunctions;
import dev.argon.argonvm.format.Argonvm;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.BiConsumer;

public class AsmParser {

	private AsmParser(String asmText, NativeFunctions nativeFunctions) {
		this.asmText = asmText;
		this.nativeFunctions = nativeFunctions;
	}

	private final String asmText;
	private final NativeFunctions nativeFunctions;
	private int index = 0;

	private static record MemberName(String typeName, String memberName) {}

	private final Map<String, Argonvm.Function> functions = new HashMap<>();
	private final List<String> functionNames = new ArrayList<>();
	private final Map<String, Argonvm.Class> classes = new HashMap<>();
	private final List<String> classNames = new ArrayList<>();

	private final Map<MemberName, Long> fieldIndexes = new HashMap<>();
	private final Map<MemberName, Long> slotIndexes = new HashMap<>();



	public static Argonvm.Program parse(String asmText, NativeFunctions nativeFunctions) throws ParseException {
		return new AsmParser(asmText, nativeFunctions).parse();
	}

	private Argonvm.Program parse() throws ParseException {

		parseTypes();
		parseFunctions();

		var entrypoint = new ChunkParser(new HashMap<>()).parseChunk();

		skipWhitespace();

		if(index < asmText.length()) {
			throw new ParseException("Unexpected remaining text: " + asmText.substring(index, index + 10));
		}

		List<Argonvm.Function> orderedFunctions = new ArrayList<>();
		for(String name : functionNames) {
			var function = functions.get(name);
			if(function == null) {
				if(nativeFunctions.get(name) != null) {
					function = Argonvm.Function.newBuilder()
						.setNative(name)
						.build();
				}
			}
			if(function == null) {
				throw new ParseException("Unknown function: " + name);
			}

			orderedFunctions.add(function);
		}

		List<Argonvm.Class> orderedClasses = new ArrayList<>();
		for(String name : classNames) {
			var cls = classes.get(name);
			if(cls == null) {
				throw new ParseException("Unknown class: " + name);
			}

			orderedClasses.add(cls);
		}

		return Argonvm.Program.newBuilder()
			.addAllFunctions(orderedFunctions)
			.addAllClasses(orderedClasses)
			.setEntrypoint(entrypoint)
			.build();
	}

	private void skipWhitespace() {
		int codepoint;
		while(index < asmText.length() && Character.isWhitespace(codepoint = asmText.codePointAt(index))) {
			index += Character.charCount(codepoint);
		}
	}

	private String peekIdentifier() {
		skipWhitespace();
		int i = index;
		int codepoint;
		while(i < asmText.length() && isValidIdentifierCharacter(codepoint = asmText.codePointAt(i))) {
			i += Character.charCount(codepoint);
		}
		if(i > index) {
			return asmText.substring(index, i);
		}
		else {
			return null;
		}
	}

	private static boolean isValidIdentifierCharacter(int codepoint) {
		return Character.isLetterOrDigit(codepoint) || codepoint == '_';
	}

	private void skipIdentifier(String id) {
		index += id.length();
	}

	private String parseIdentifier() {
		String id = peekIdentifier();
		skipIdentifier(id);
		return id;
	}

	private boolean tryParseSymbol(String symbol) throws ParseException {
		skipWhitespace();
		boolean result = asmText.startsWith(symbol, index);
		if(result) {
			index += symbol.length();
		}
		return result;
	}

	private void parseSymbol(String symbol) throws ParseException {
		if(!tryParseSymbol(symbol)) {
			String found = index < asmText.length() ? asmText.substring(index, index + 10) : "EOF";
			throw new ParseException("Expected '" + symbol + "', found '" + found + "'");
		}
	}

	private void parseTypes() throws ParseException {
		while(true) {
			String id = peekIdentifier();
			if(id == null) {
				break;
			}
			else if(id.equals("CLASS")) {
				skipIdentifier(id);
				parseRemainingClass();
			}
			else {
				break;
			}
		}
	}

	private void parseFunctions() throws ParseException {
		while(true) {
			String id = peekIdentifier();
			if(id == null) {
				break;
			}
			else if(id.equals("FUNCTION")) {
				skipIdentifier(id);
				parseRemainingFunction();
			}
			else {
				break;
			}
		}
	}

	private void parseRemainingFunction() throws ParseException {
		var bytecodeFunction = Argonvm.BytecodeFunction.newBuilder();

		String name = parseIdentifier();
		parseSymbol("(");
		Map<String, Integer> variableIndexes = new HashMap<>();

		String paramName = peekIdentifier();
		if(paramName != null) {
			skipIdentifier(paramName);

			parseSymbol(":");
			var paramType = parseType();
			bytecodeFunction.addParameterTypes(paramType);
			variableIndexes.put(paramName, variableIndexes.size());

			while(tryParseSymbol(",")) {
				paramName = parseIdentifier();
				parseSymbol(":");
				paramType = parseType();
				bytecodeFunction.addParameterTypes(paramType);
				variableIndexes.put(paramName, variableIndexes.size());
			}
		}
		parseSymbol(")");
		parseSymbol(":");
		bytecodeFunction.setReturnType(parseType());
		bytecodeFunction.setBody(new ChunkParser(variableIndexes).parseChunk());

		if(!parseIdentifier().equals("END")) {
			throw new ParseException();
		}

		var function = Argonvm.Function.newBuilder()
			.setBytecode(bytecodeFunction)
			.build();

		functions.put(name, function);
	}

	private void parseRemainingClass() throws ParseException {
		var classDecl = Argonvm.Class.newBuilder();

		String name = parseIdentifier();

		if(tryParseSymbol(":")) {
			String baseClassName = parseIdentifier();
			long baseClassIndex = getClassIndex(baseClassName);
			classDecl.setBaseClassId(baseClassIndex);
		}

		classBodyLoop:
		while(true) {
			var id = parseIdentifier();
			switch(id) {
				case "END" -> {
					break classBodyLoop;
				}
				case "FIELD" -> {
					String fieldName = parseIdentifier();
					parseSymbol(":");
					var t = parseType();
					long fieldIndex = classDecl.getFieldsCount();
					classDecl.addFields(
						Argonvm.Field.newBuilder()
							.setType(t)
					);
					fieldIndexes.put(new MemberName(name, fieldName), fieldIndex);
				}
				case "SLOT" -> {
					var slot = Argonvm.MethodSlot.newBuilder();

					String slotName = parseIdentifier();

					parseSymbol("(");

					if(!tryParseSymbol(")")) {
						var paramType = parseType();
						slot.addParameterTypes(paramType);

						while(tryParseSymbol(",")) {
							paramType = parseType();
							slot.addParameterTypes(paramType);
						}

						parseSymbol(")");
					}

					parseSymbol(":");
					var returnType = parseType();
					slot.setReturnType(returnType);

					long slotIndex = classDecl.getSlotsCount();
					classDecl.addSlots(slot);
					slotIndexes.put(new MemberName(name, slotName), slotIndex);
				}
				case "IMPL_CLASS" -> {
					var impl = Argonvm.MethodImplementation.newBuilder();
					var className = parseIdentifier();
					parseSymbol(".");
					var memberName = parseIdentifier();
					parseSymbol("=");
					var functionName = parseIdentifier();

					long declaringClass = getClassIndex(className);
					long slotIndex = slotIndexes.get(new MemberName(className, memberName));
					long functionIndex = getFunctionIndex(functionName);

					classDecl.addImplementations(
						Argonvm.MethodImplementation.newBuilder()
							.setDeclaringClassId(declaringClass)
							.setSlotIndex(slotIndex)
							.setFunctionIndex(functionIndex)
					);
				}
				default -> throw new ParseException();
			}
		}

		classes.put(name, classDecl.build());
	}

	private Argonvm.ValueType parseType() throws ParseException {
		var id = parseIdentifier();
		return switch(id) {
			case "INT8" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_INT8)
				.build();


			case "INT16" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_INT16)
				.build();


			case "INT32" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_INT32)
				.build();


			case "INT64" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_INT64)
				.build();


			case "FLOAT32" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_FLOAT32)
				.build();


			case "FLOAT64" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_FLOAT64)
				.build();


			case "OBJECT_REFERENCE" -> Argonvm.ValueType.newBuilder()
				.setSimple(Argonvm.ValueTypeSimple.VALUE_TYPE_SIMPLE_OBJECT_REFERENCE)
				.build();


			case "TUPLE" -> {
				parseSymbol("(");

				var tuple = Argonvm.ValueTypeTuple.newBuilder();
				if(!tryParseSymbol(")")) {
					tuple.addElements(parseType());
					while(tryParseSymbol(",")) {
						tuple.addElements(parseType());
					}
					parseSymbol(")");
				}

				yield Argonvm.ValueType.newBuilder()
					.setTuple(tuple)
					.build();
			}

			default -> {
				throw new ParseException("Unexpected type: " + id);
			}
		};
	}


	private final class ChunkParser {
		public ChunkParser(Map<String, Integer> variableIndexes) {
			this.variableIndexes = variableIndexes;
		}

		Argonvm.Chunk.Builder chunk = Argonvm.Chunk.newBuilder();
		Map<String, Integer> variableIndexes;
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		List<JumpTarget> jumpTargets = new ArrayList<>();
		Map<String, Integer> labelAddresses = new HashMap<>();

		public static record JumpTarget(int offset, String targetLabel) {}



		public Argonvm.Chunk parseChunk() throws ParseException {
			String opName;
			while((opName = peekIdentifier()) != null) {
				if(opName.equals("END")) {
					break;
				}

				skipIdentifier(opName);

				if(tryParseSymbol(":")) {
					labelAddresses.put(opName, os.size());
					continue;
				}

				if(opName.equals("LOCAL")) {
					String id = parseIdentifier();
					parseSymbol(":");
					var t = parseType();
					chunk.addVariableTypes(t);
					variableIndexes.put(id, variableIndexes.size());
					continue;
				}

				Instruction insn = switch(opName) {
					case "NOP" -> new Instruction.NOP();
					case "POP" -> new Instruction.POP();
					case "RETURN" -> new Instruction.RETURN();
					case "CONSTANT" -> {
						var t = parseType();

						int index = chunk.getConstantsCount();
						var constant = parseConstant(t);
						chunk.addConstants(constant);
						yield new Instruction.CONSTANT(index);
					}
					case "NEGATE" -> new Instruction.NEGATE();
					case "ADD" -> new Instruction.ADD();
					case "SUBTRACT" -> new Instruction.SUBTRACT();
					case "MULTIPLY" -> new Instruction.MULTIPLY();
					case "DIVIDE" -> new Instruction.DIVIDE();
					case "DIVIDE_UN" -> new Instruction.DIVIDE_UN();
					case "CONSTANT_0_INT32" -> new Instruction.CONSTANT_0_INT32();
					case "CONSTANT_1_INT32" -> new Instruction.CONSTANT_1_INT32();
					case "CONSTANT_NULL" -> new Instruction.CONSTANT_NULL();
					case "EQZ" -> new Instruction.EQZ();
					case "NEZ" -> new Instruction.NEZ();
					case "EQ" -> new Instruction.EQ();
					case "NE" -> new Instruction.NE();
					case "LT" -> new Instruction.LT();
					case "LT_UN" -> new Instruction.LT_UN();
					case "GT" -> new Instruction.GT();
					case "GT_UN" -> new Instruction.GT_UN();
					case "JMP16" -> parseJump16(Instruction.JMP16::new);
					case "JZ16" -> parseJump16(Instruction.JZ16::new);
					case "JNZ16" -> parseJump16(Instruction.JNZ16::new);
					case "CALL" -> parseCall(Instruction.CALL::new);
					case "RETURN_CALL" -> parseCall(Instruction.RETURN_CALL::new);
					case "LD_LOCAL" -> parseVariableInstruction(Instruction.LD_LOCAL::new);
					case "ST_LOCAL" -> parseVariableInstruction(Instruction.ST_LOCAL::new);
					case "NEW" -> {
						var className = parseIdentifier();
						long index = getClassIndex(className);
						yield new Instruction.NEW(index);
					}
					case "LD_FIELD" -> parseFieldInstruction(Instruction.LD_FIELD::new);
					case "ST_FIELD" -> parseFieldInstruction(Instruction.ST_FIELD::new);
					case "CALL_CLASS" -> parseCallClass(Instruction.CALL_CLASS::new);
					case "RETURN_CALL_CLASS" -> parseCallClass(Instruction.RETURN_CALL_CLASS::new);
					default -> throw new ParseException("Unexpected instruction: " + opName);
				};

				try {
					insn.write(os);
				}
				catch(IOException ex) {
					throw new ParseException();
				}
			}

			byte[] bytecode = os.toByteArray();

			for(var jumpTarget : jumpTargets) {
				int address = labelAddresses.get(jumpTarget.targetLabel());
				int offset = jumpTarget.offset();
				bytecode[offset] = (byte)address;
				bytecode[offset + 1] = (byte)(address >>> 8);
			}

			chunk.setBytecode(ByteString.copyFrom(bytecode));
			return chunk.build();
		}

		private Argonvm.ConstantValue parseConstant(Argonvm.ValueType t) throws ParseException {
			if(t.hasSimple()) {
				return switch(t.getSimple()) {
					case VALUE_TYPE_SIMPLE_INT8 -> this.<Byte, Integer>parseIntLiteral(Byte::valueOf, Byte::intValue, Argonvm.ConstantValue.Builder::setInt8);
					case VALUE_TYPE_SIMPLE_INT16 -> this.<Short, Integer>parseIntLiteral(Short::valueOf, Short::intValue, Argonvm.ConstantValue.Builder::setInt16);
					case VALUE_TYPE_SIMPLE_INT32 -> this.<Integer, Integer>parseIntLiteral(Integer::valueOf, Function.identity(), Argonvm.ConstantValue.Builder::setInt32);
					case VALUE_TYPE_SIMPLE_INT64 -> this.<Long, Long>parseIntLiteral(Long::valueOf, Function.identity(), Argonvm.ConstantValue.Builder::setInt64);
					case VALUE_TYPE_SIMPLE_FLOAT32 -> this.<Float>parseFloatLiteral(Float::valueOf, Argonvm.ConstantValue.Builder::setFloat32);
					case VALUE_TYPE_SIMPLE_FLOAT64 -> this.<Double>parseFloatLiteral(Double::valueOf, Argonvm.ConstantValue.Builder::setFloat64);
					case VALUE_TYPE_SIMPLE_OBJECT_REFERENCE -> {
						String objType = parseIdentifier();
						switch(objType) {
							case "STRING" -> {
								String value = parseStringLiteral();
								yield Argonvm.ConstantValue.newBuilder()
									.setStringLiteral(value)
									.build();
							}
							default -> {
								throw new ParseException();
							}
						}
					}
				};
			}
			else if(t.hasTuple()) {
				List<Argonvm.ConstantValue> values = new ArrayList<>();
				for(var element : t.getTuple().getElementsList()) {
					values.add(parseConstant(element));
				}
				return Argonvm.ConstantValue.newBuilder()
					.setTuple(
						Argonvm.TupleValue.newBuilder()
							.addAllElements(values)
					)
					.build();
			}
			else {
				throw new ParseException();
			}
		}

		private <T, U> Argonvm.ConstantValue parseIntLiteral(Function<String, T> parse, Function<T, U> widen, BiConsumer<Argonvm.ConstantValue.Builder, T> setValue) throws ParseException {
			skipWhitespace();
			int start = index;
			int codepoint;
			if(index < asmText.length() && (codepoint = asmText.codePointAt(index)) == '-') {
				index += 1;
			}

			while(index < asmText.length() && Character.isDigit(codepoint = asmText.codePointAt(index))) {
				index += Character.charCount(codepoint);
			}

			if(index <= start) {
				throw new ParseException();
			}

			T value;
			try {
				value = parse.apply(asmText.substring(start, index));
			}
			catch(NumberFormatException ex) {
				throw new ParseException();
			}

			var builder = Argonvm.ConstantValue.newBuilder();
			setValue.accept(builder, value);
			return builder.build();
		}

		private <T> Argonvm.ConstantValue parseFloatLiteral(Function<String, T> parse, BiConsumer<Argonvm.ConstantValue.Builder, T> setValue) throws ParseException {
			skipWhitespace();
			int start = index;
			int codepoint;
			if(index < asmText.length() && (codepoint = asmText.codePointAt(index)) == '-') {
				index += 1;
			}

			while(index < asmText.length() && isValidFloatChar(codepoint = asmText.codePointAt(index))) {
				index += Character.charCount(codepoint);
			}

			if(index <= start) {
				throw new ParseException();
			}

			T value;
			try {
				value = parse.apply(asmText.substring(start, index));
			}
			catch(NumberFormatException ex) {
				throw new ParseException();
			}

			var builder = Argonvm.ConstantValue.newBuilder();
			setValue.accept(builder, value);
			return builder.build();
		}

		private boolean isValidFloatChar(int codepoint) {
			return Character.isDigit(codepoint) || codepoint == '.';
		}

		private String parseStringLiteral() throws ParseException {
			parseSymbol("\"");

			StringBuffer sb = new StringBuffer();
			char ch;
			for(; index < asmText.length() && (ch = asmText.charAt(index)) != '"'; ++index) {
				if(ch == '\\') {
					++index;

					if(index >= asmText.length()) {
						throw new ParseException();
					}

					ch = asmText.charAt(index);
				}

				sb.append(ch);
			}

			if(index >= asmText.length() || asmText.charAt(index) != '"') {
				throw new RuntimeException();
			}

			++index;

			return sb.toString();
		}

		private Instruction parseJump16(Function<Short, Instruction> create) throws ParseException {
			String label = parseIdentifier();
			int offset = os.size();
			short dummyTarget = 0;
			Instruction insn = create.apply(dummyTarget);
			offset += insn.opcodeWidth();

			jumpTargets.add(new JumpTarget(offset, label));
			return insn;
		}

		private Instruction parseCall(Function<Long, Instruction> create) throws ParseException {
			String name = parseIdentifier();
			long index = getFunctionIndex(name);
			return create.apply(index);
		}

		private Instruction parseVariableInstruction(Function<Long, Instruction> create) throws ParseException {
			String name = parseIdentifier();
			long index = variableIndexes.get(name);
			return create.apply(index);
		}

		private Instruction parseFieldInstruction(BiFunction<Long, Long, Instruction> create) throws ParseException {
			var className = parseIdentifier();
			parseSymbol(".");
			var fieldName = parseIdentifier();

			long classId = getClassIndex(className);

			long index = fieldIndexes.get(new MemberName(className, fieldName));

			return create.apply(classId, index);
		}

		private Instruction parseCallClass(BiFunction<Long, Long, Instruction> create) throws ParseException {
			var className = parseIdentifier();
			parseSymbol(".");
			var slotName = parseIdentifier();

			long classId = getClassIndex(className);

			long index = slotIndexes.get(new MemberName(className, slotName));

			return create.apply(classId, index);
		}
	}

	private static long getIndexFromName(String name, List<String> names) {
		int index = names.indexOf(name);
		if(index < 0) {
			index = names.size();
			names.add(name);
		}
		return index;
	}

	private long getFunctionIndex(String name) {
		return getIndexFromName(name, functionNames);
	}

	private long getClassIndex(String name) {
		return getIndexFromName(name, classNames);
	}




}
