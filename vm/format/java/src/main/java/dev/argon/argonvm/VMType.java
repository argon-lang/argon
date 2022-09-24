package dev.argon.argonvm;

import java.util.List;

public sealed interface VMType {
	Object defaultValue();

	public static record Int8() implements VMType {
		@Override
		public Object defaultValue() {
			byte b = 0;
			return Byte.valueOf(b);
		}
	}
	public static record Int16() implements VMType {
		@Override
		public Object defaultValue() {
			short s = 0;
			return Short.valueOf(s);
		}
	}
	public static record Int32() implements VMType {
		@Override
		public Object defaultValue() {
			return 0;
		}
	}
	public static record Int64() implements VMType {
		@Override
		public Object defaultValue() {
			return 0L;
		}
	}
	public static record Float32() implements VMType {
		@Override
		public Object defaultValue() {
			return 0.0f;
		}
	}
	public static record Float64() implements VMType {
		@Override
		public Object defaultValue() {
			return 0.0;
		}
	}
	public static record Tuple(List<VMType> elements) implements VMType {
		@Override
		public Object defaultValue() {
			var values = new Object[elements.size()];
			for(int i = 0; i < elements.size(); ++i) {
				values[i] = elements.get(i).defaultValue();
			}
			return new VMTuple(values);
		}
	}
	public static record ObjectReference() implements VMType {
		@Override
		public Object defaultValue() {
			return null;
		}
	}
}
