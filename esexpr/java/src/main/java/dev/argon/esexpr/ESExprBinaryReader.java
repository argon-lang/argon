package dev.argon.esexpr;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class ESExprBinaryReader {
	public ESExprBinaryReader(@NotNull List<? extends @NotNull String> symbolTable, @NotNull InputStream is) {
		this.symbolTable = symbolTable;
		this.is = is;
	}

	private final List<? extends @NotNull String> symbolTable;
	private final @NotNull InputStream is;
	private int nextByte;

	public @Nullable ESExpr read() throws IOException, SyntaxException {
		if(peekNext() < 0) {
			return null;
		}

		return readExpr();
	}

	public @NotNull List<@NotNull ESExpr> readAll() throws IOException, SyntaxException {
		var exprs = new ArrayList<@NotNull ESExpr>();
		while(peekNext() >= 0) {
			exprs.add(readExpr());
		}
		return exprs;
	}


	private int next() throws IOException {
		if(nextByte >= 0) {
			int res = nextByte;
			nextByte = -1;
			return res;
		}

		return is.read();
	}

	private int peekNext() throws IOException {
		if(nextByte >= 0) {
			return nextByte;
		}

		nextByte = is.read();
		return nextByte;
	}

	private BinToken nextToken() throws IOException, SyntaxException {
		int b = next();
		if(b < 0) {
			throw new EOFException();
		}

		BinToken.WithIntegerType type = switch(b & 0xE0) {
			case 0x00 -> BinToken.WithIntegerType.CONSTRUCTOR;
			case 0x20 -> BinToken.WithIntegerType.INT;
			case 0x40 -> BinToken.WithIntegerType.NEG_INT;
			case 0x60 -> BinToken.WithIntegerType.STRING;
			case 0x80 -> BinToken.WithIntegerType.BINARY;
			case 0xA0 -> BinToken.WithIntegerType.KWARG;
			case 0xC0 -> throw new SyntaxException();
			default -> null;
		};

		if(type == null) {
			return switch(b) {
				case 0xE0 -> BinToken.Fixed.NULL;
				case 0xE1 -> BinToken.Fixed.CONSTRUCTOR_END;
				case 0xE2 -> BinToken.Fixed.TRUE;
				case 0xE3 -> BinToken.Fixed.FALSE;
				case 0xE4 -> BinToken.Fixed.FLOAT32;
				case 0xE5 -> BinToken.Fixed.FLOAT64;
				default -> throw new SyntaxException();
			};
		}
		else {
			BigInteger i = BigInteger.valueOf(b & 0x0F);
			if((b & 0x10) == 0x10) {
				i = readInt(i, 4);
			}

			return new BinToken.WithInteger(type, i);
		}
	}


	private BigInteger readInt() throws IOException {
		return readInt(BigInteger.ZERO, 0);
	}

	private BigInteger readInt(BigInteger acc, int bits) throws IOException {
		while(true) {
			int b = next();
			if(b < 0) {
				throw new EOFException();
			}

			acc = acc.or(BigInteger.valueOf(b & 0x7F).shiftLeft(bits));
			bits += 7;

			if((b & 0x80) == 0x00) {
				return acc;
			}
		}
	}



	private @NotNull ESExpr readExpr() throws SyntaxException, IOException {
		if(readExprPlus() instanceof ESExprPlus.Simple(var e)) {
			return e;
		}
		else {
			throw new SyntaxException();
		}
	}

	sealed interface ESExprPlus {
		record Simple(ESExpr e) implements ESExprPlus {}
		record KeywordArg(String sym, ESExpr e) implements ESExprPlus {}
		record ConstructorEnd() implements ESExprPlus {}
	}


	private @NotNull ESExprPlus readExprPlus() throws SyntaxException, IOException {
		return switch(nextToken()) {
			case BinToken.WithInteger(var type, var value) -> switch(type) {
				case CONSTRUCTOR -> {
					var sym = symbolTable.get(value.intValueExact());
					var kwargs = new HashMap<String, ESExpr>();
					var args = new ArrayList<ESExpr>();
					argLoop:
					while(true) {
						switch(readExprPlus()) {
							case ESExprPlus.Simple(var e) -> args.add(e);
							case ESExprPlus.KeywordArg(var kwsym, var e) -> kwargs.put(kwsym, e);
							case ESExprPlus.ConstructorEnd() -> {
								break argLoop;
							}
						}
					}

					yield new ESExprPlus.Simple(
						new ESExpr.Constructed(sym, Collections.unmodifiableMap(kwargs), Collections.unmodifiableList(args))
					);
				}
				case INT -> new ESExprPlus.Simple(new ESExpr.Int(value));
				case NEG_INT -> new ESExprPlus.Simple(new ESExpr.Int(value.add(BigInteger.ONE).negate()));

				// Should be safe to bypass next/peekNext here.
				case STRING -> {
					int len = value.intValueExact();
					byte[] b = new byte[len];
					if(is.readNBytes(b, 0, len) < len) {
						throw new EOFException();
					}

					yield new ESExprPlus.Simple(new ESExpr.Str(new String(b, StandardCharsets.UTF_8)));
				}

				case BINARY -> {
					int len = value.intValueExact();
					byte[] b = new byte[len];
					if(is.readNBytes(b, 0, len) < len) {
						throw new EOFException();
					}

					yield new ESExprPlus.Simple(new ESExpr.Binary(b));
				}

				case KWARG -> {
					var sym = symbolTable.get(value.intValueExact());
					var memberValue = readExpr();
					yield new ESExprPlus.KeywordArg(sym, memberValue);
				}
			};

			case BinToken.Fixed fixed -> switch(fixed) {
				case NULL -> new ESExprPlus.Simple(new ESExpr.Null());
				case CONSTRUCTOR_END -> new ESExprPlus.ConstructorEnd();
				case TRUE -> new ESExprPlus.Simple(new ESExpr.Bool(true));
				case FALSE -> new ESExprPlus.Simple(new ESExpr.Bool(false));
				case FLOAT32 -> {
					int bits = 0;
					for(int i = 0; i < 4; ++i) {
						int b = next();
						if(b < 0) {
							throw new EOFException();
						}

						bits |= (b & 0xFF) << (i * 8);
					}

					yield new ESExprPlus.Simple(new ESExpr.Float32(Float.intBitsToFloat(bits)));
				}
				case FLOAT64 -> {
					long bits = 0;
					for(int i = 0; i < 8; ++i) {
						int b = next();
						if(b < 0) {
							throw new EOFException();
						}

						bits |= (long)(b & 0xFF) << (i * 8);
					}

					yield new ESExprPlus.Simple(new ESExpr.Float64(Double.longBitsToDouble(bits)));
				}
			};
		};
	}

}
