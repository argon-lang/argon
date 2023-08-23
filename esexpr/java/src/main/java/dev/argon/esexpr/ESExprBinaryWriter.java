package dev.argon.esexpr;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class ESExprBinaryWriter {

	public ESExprBinaryWriter(@NotNull List<? extends @NotNull String> symbolTable, OutputStream os) {

		this.symbolTable = symbolTable;
		this.os = os;
	}

	private final List<? extends @NotNull String> symbolTable;
	private final OutputStream os;

	public void write(ESExpr expr) throws IOException {
		switch(expr) {
			case ESExpr.Constructed(var constructor, var kwargs, var args) -> {
				var index = getSymbolIndex(constructor);
				writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.CONSTRUCTOR, index));
				for(var entry : kwargs.entrySet()) {
					var index2 = getSymbolIndex(entry.getKey());
					writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.KWARG, index2));
					write(entry.getValue());
				}
				for(var arg : args) {
					write(arg);
				}
				writeToken(BinToken.Fixed.CONSTRUCTOR_END);
			}

			case ESExpr.Bool(var b) -> {
				if(b) {
					writeToken(BinToken.Fixed.TRUE);
				}
				else {
					writeToken(BinToken.Fixed.FALSE);
				}
			}

			case ESExpr.Int(var i) -> {
				if(i.signum() < 0) {
					writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.NEG_INT, i.negate().subtract(BigInteger.ONE)));
				}
				else {
					writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.INT, i));
				}
			}

			case ESExpr.Str(var s) -> {
				byte[] b = s.getBytes(StandardCharsets.UTF_8);
				writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.STRING, BigInteger.valueOf(b.length)));
				os.write(b);
			}

			case ESExpr.Binary(var b) -> {
				writeToken(new BinToken.WithInteger(BinToken.WithIntegerType.BINARY, BigInteger.valueOf(b.length)));
				os.write(b);
			}

			case ESExpr.Float32(var f) -> {
				writeToken(BinToken.Fixed.FLOAT32);
				int bits = Float.floatToRawIntBits(f);
				for(int i = 0; i < 4; ++i) {
					os.write(bits & 0xFF);
					bits >>>= 8;
				}
			}

			case ESExpr.Float64(var d) -> {
				writeToken(BinToken.Fixed.FLOAT64);
				long bits = Double.doubleToRawLongBits(d);
				for(int i = 0; i < 8; ++i) {
					os.write((int)bits & 0xFF);
					bits >>>= 8;
				}
			}

			case ESExpr.Null() -> {
				writeToken(BinToken.Fixed.NULL);
			}
		}
	}

	private void writeToken(BinToken token) throws IOException {
		switch(token) {
			case BinToken.WithInteger(var type, var value) -> {
				int b = switch(type) {
					case CONSTRUCTOR -> 0x00;
					case INT -> 0x20;
					case NEG_INT -> 0x40;
					case STRING -> 0x60;
					case BINARY -> 0x80;
					case KWARG -> 0xA0;
				};

				b |= value.byteValue() & 0x40;
				value = value.shiftRight(4);

				boolean isPos = value.signum() > 0;
				if(isPos) {
					b |= 0x10;
				}
				os.write(b);
				if(isPos) {
					writeInt(value);
				}
			}
			case BinToken.Fixed fixed -> {
				int b = switch(fixed) {
					case NULL -> 0xE0;
					case CONSTRUCTOR_END -> 0xE1;
					case TRUE -> 0xE2;
					case FALSE -> 0xE3;
					case FLOAT32 -> 0xE4;
					case FLOAT64 -> 0xE5;
				};
			}
		}
	}

	private BigInteger getSymbolIndex(String symbol) {
		int index = symbolTable.indexOf(symbol);
		if(index < 0) {
			throw new IndexOutOfBoundsException();
		}
		return BigInteger.valueOf(index);
	}

	private void writeInt(BigInteger value) throws IOException {
		do {
			int b = value.byteValue() & 0x7F;
			value = value.shiftRight(7);

			if(value.signum() > 0) {
				b |= 0x80;
			}
			os.write(b);
		} while(value.signum() > 0);
	}

}
