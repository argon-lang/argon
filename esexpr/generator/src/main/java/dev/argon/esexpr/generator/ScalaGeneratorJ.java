package dev.argon.esexpr.generator;

import dev.argon.esexpr.ESExpr;
import org.apache.commons.text.StringEscapeUtils;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;

class ScalaGeneratorJ {

	public static void writeValue(@NotNull PrintWriter writer, @NotNull ESExpr value) throws IOException {
		switch(value) {
			case ESExpr.Constructed constructed -> {
//				writer.write(nameMap.get(constructed.constructor()));
//				writer.write("(");
//
				throw new RuntimeException("Not implemented");
			}
			case ESExpr.Bool(var b) -> writer.write(Boolean.toString(b));
			case ESExpr.Int(var i) -> writer.write(i.toString());
			case ESExpr.Str(var s) -> {
				writer.write("\"");
				writer.write(StringEscapeUtils.escapeJava(s));
				writer.write("\"");
			}
			case ESExpr.Binary(var b) -> {
				writer.write("Array[Byte](");
				for(int i = 0; i < b.length; ++i) {
					writer.write(Byte.toString(b[i]));
					if(i < b.length - 1) {
						writer.write(", ");
					}
				}
				writer.write(")");
			}
			case ESExpr.Float32(var f) -> {
				writer.write("java.lang.Float.valueOf(\"");
				writer.write(Float.toHexString(f));
				writer.write("\")");
			}
			case ESExpr.Float64(var f) -> {
				writer.write("java.lang.Double.valueOf(\"");
				writer.write(Double.toHexString(f));
				writer.write("\")");
			}
			case ESExpr.Null() -> writer.write("null");
		}
	}

}
