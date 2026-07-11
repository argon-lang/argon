package dev.argon.backend.ir;

import dev.argon.backend.codegen.ProgramModel;
import dev.argon.vm.BuiltinType;
import dev.argon.vm.IntegerType;
import dev.argon.vm.Token;

import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;

public final class TokenTypes {

	public static boolean elementTypeRequiresErasedArray(Token elementType) {
		return switch(elementType) {
			case Token.TokenParameter _, Token.ParentTokenParameter _ -> true;
			default -> false;
		};
	}


	public static ClassDesc tokenAsClassDesc(ProgramModel program, Token token) {
		return switch(token) {
			case Token.Boxed _ -> ConstantDescs.CD_Object;
			case Token.Builtin(var bt) -> switch(bt) {
				case BuiltinType.Array(var elementType) ->
					elementTypeRequiresErasedArray(elementType) ?
						ConstantDescs.CD_Object :
						tokenAsClassDesc(program, elementType).arrayType();
				case BuiltinType.Bool() -> ConstantDescs.CD_boolean;
				case BuiltinType.Conjunction _ -> throw new RuntimeException("Conjunction not implemented");
				case BuiltinType.Disjunction _ -> throw new RuntimeException("Disjunction not implemented");
				case BuiltinType.Int(var integerType) -> switch(integerType) {
					case INT -> ClassDesc.of("java.math.BigInteger");
					case U8 -> ConstantDescs.CD_byte;
				};
				case BuiltinType.Never() -> ClassDesc.of("dev.argon.runtime.Never");
				case BuiltinType.String() -> ConstantDescs.CD_String;
			};
			case Token.Enum enumToken -> {
				var enumInfo = program.getEnumInfo(enumToken.enumId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, enumInfo.importSpecifier());
			}
			case Token.Function _ -> ClassDesc.of("dev.argon.runtime.Function");
			case Token.FunctionErased _ -> ClassDesc.of("dev.argon.runtime.FunctionErased");
			case Token.FunctionToken _ -> ClassDesc.of("dev.argon.runtime.FunctionToken");
			case Token.InstanceType instanceType -> {
				var instanceInfo = program.getInstanceInfo(instanceType.instanceId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, instanceInfo.importSpecifier());
			}
			case Token.InstanceValue _ -> {
				throw new UnsupportedOperationException("InstanceValue cannot be used as a type");
			}
			case Token.ParentTokenParameter _ -> ConstantDescs.CD_Object;
			case Token.Record record -> {
				var recordInfo = program.getRecordInfo(record.recordId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, recordInfo.importSpecifier());
			}
			case Token.RefCell _ -> ClassDesc.of("dev.argon.runtime.RefCell");
			case Token.TokenParameter _ -> ConstantDescs.CD_Object;
			case Token.Trait trait -> {
				var traitInfo = program.getTraitInfo(trait.traitId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, traitInfo.importSpecifier());
			}
			case Token.Tuple tuple -> {
				if(tuple.elements().size() > 10) {
					yield ClassDesc.of("dev.argon.runtime.TupleXL");
				} else {
					yield ClassDesc.of("dev.argon.runtime.Tuple" + tuple.elements().size());
				}
			}
			case Token.TypeInfo() -> ClassDesc.of("dev.argon.runtime.TypeInfo");
		};
	}
}
