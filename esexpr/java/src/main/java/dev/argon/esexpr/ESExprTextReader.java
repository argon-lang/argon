package dev.argon.esexpr;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.EOFException;
import java.io.IOException;
import java.io.Reader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public final class ESExprTextReader {

    public ESExprTextReader(@NotNull Reader reader) {
        this.reader = reader;
    }

    private final @NotNull Reader reader;
	private int nextChar = -1;


    public @Nullable ESExpr read() throws IOException, SyntaxException {
		skipWhitespace();
		if(peekNext() < 0) {
			return null;
		}

        return readExpr();
    }

	public @NotNull List<@NotNull ESExpr> readAll() throws IOException, SyntaxException {
		var exprs = new ArrayList<@NotNull ESExpr>();
		while(true) {
			skipWhitespace();
			if(peekNext() < 0) {
				break;
			}

			exprs.add(readExpr());
		}
		return exprs;
	}


	private int next() throws IOException {
		if(nextChar >= 0) {
			int res = nextChar;
			nextChar = -1;
			return res;
		}

		return reader.read();
	}

	private int peekNext() throws IOException {
		if(nextChar >= 0) {
			return nextChar;
		}

		nextChar = reader.read();
		return nextChar;
	}

	public void skipWhitespace() throws IOException {
		while(true) {
			int ch = peekNext();
			if(ch < 0) {
				break;
			}
			else if(Character.isWhitespace((char)ch)) {
				next();
			}
			else {
				break;
			}
		}
	}


	public @NotNull ESExpr readExpr() throws IOException, SyntaxException {
		skipWhitespace();
		int ch = next();
		if(ch < 0) {
			throw new EOFException();
		}

		switch(ch) {
			case '(' -> {
				var sym = readSymbol();
				var kwargs = new HashMap<String, ESExpr>();
				var args = new ArrayList<ESExpr>();
				while(true) {
					skipWhitespace();
					int nextCh = peekNext();
					if(nextCh == ')') {
						next();
						break;
					}
					else if(nextCh >= 0 && (Character.isLetter((char)nextCh) || SYMBOLS.indexOf((char)nextCh) >= 0)) {
						var kwargSym = readSymbol();
						skipWhitespace();
						nextCh = next();
						if(nextCh != ':') {
							throw new SyntaxException();
						}

						var arg = readExpr();

						if(kwargs.putIfAbsent(kwargSym, arg) != null) {
							throw new SyntaxException();
						}
					}
					else {
						var arg = readExpr();
						args.add(arg);
					}
				}

				return new ESExpr.Constructed(sym, Collections.unmodifiableMap(kwargs), Collections.unmodifiableList(args));
			}

			case '"' -> {
				var sb = new StringBuilder();
				while(true) {
					ch = next();
					if(ch == '"') {
						break;
					}
					else if(ch == '\\') {
						ch = next();
						if(ch < 0) {
							throw new SyntaxException();
						}

						switch(ch) {
							case 'f' -> sb.append('\f');
							case 'n' -> sb.append('\n');
							case 'r' -> sb.append('\r');
							case 't' -> sb.append('\t');
							case '\\' -> sb.append('\\');
							case '\'' -> sb.append('\'');
							case '"' -> sb.append('"');
							case 'u' -> {
								if(next() != '{') {
									throw new SyntaxException();
								}
								var sbCodepoint = new StringBuilder();
								while(true) {
									ch = next();
									if(ch < 0) {
										throw new SyntaxException();
									}
									else if(Character.isDigit((char)ch)) {
										sbCodepoint.append((char)ch);
									}
									else if(ch == '}') {
										break;
									}
									else {
										throw new SyntaxException();
									}
								}

								int codepoint;
								try {
									codepoint = Integer.parseInt(sbCodepoint.toString());
								}
								catch(NumberFormatException ex) {
									throw new SyntaxException();
								}

								sb.append(Character.toString(codepoint));
							}
							default -> throw new SyntaxException();
						}
					}
					else {
						sb.append((char)ch);
					}
				}

				return new ESExpr.Str(sb.toString());
			}

			case '-' -> {
				return readNumber('-');
			}

			case '#' -> {
				var sym = readSymbol();
				return switch(sym) {
					case "T", "true" -> new ESExpr.Bool(true);
					case "F", "false" -> new ESExpr.Bool(false);
					case "null" -> new ESExpr.Null();
					default -> throw new SyntaxException();
				};
			}

			default -> {
				if(Character.isDigit((char)ch)) {
					return readNumber((char)ch);
				}
				
				throw new SyntaxException();
			}
		}
	}

	private static final String SYMBOLS = "+-_*/%";

	private @NotNull String readSymbol() throws IOException, SyntaxException {
		skipWhitespace();

		var sb = new StringBuilder();

		var ch = next();
		if(ch < 0) {
			throw new SyntaxException();
		}

		if(Character.isLetter((char)ch)) {
			sb.append((char)ch);
		}
		else if(SYMBOLS.indexOf((char)ch) >= 0) {
			sb.append((char)ch);
			ch = peekNext();
			if(ch >= 0 && Character.isDigit((char)ch)) {
				throw new SyntaxException();
			}
		}
		else {
			throw new SyntaxException();
		}

		while(true) {
			ch = peekNext();
			if(ch < 0 || !(Character.isLetterOrDigit((char)ch) || SYMBOLS.indexOf((char)ch) >= 0)) {
				break;
			}

			sb.append((char)next());
		}

		return sb.toString();
	}

	private @NotNull ESExpr readNumber(char startCh) throws IOException, SyntaxException {
		skipWhitespace();

		var sb = new StringBuilder();
		boolean negative = false;
		boolean isFloat = false;
		boolean isHex = false;
		boolean foundExponent = false;
		boolean foundSuffix = false;

		int ch;

		if(startCh == '-') {
			sb.append('-');
			ch = next();
		}
		else {
			ch = startCh;
		}

		if(ch < 0) {
			throw new SyntaxException();
		}
		else if(ch == '0') {
			sb.append((char)ch);
			ch = peekNext();
			if(ch >= 0) {
				if(ch == 'X' || ch == 'x') {
					sb.append((char)next());
					isHex = true;
				}
			}
		}
		else if(Character.isDigit((char)ch)) {
			sb.append((char)ch);
		}
		else {
			throw new SyntaxException();
		}

		while(!foundSuffix) {
			ch = peekNext();

			if(ch < 0) {
				break;
			}
			else if((!isHex || foundExponent) && (ch == 'f' || ch == 'F')) {
				isFloat = true;
				foundSuffix = true;
				sb.append((char)next());
			}
			else if(foundExponent) {
				if(Character.isWhitespace((char)ch)) {
					break;
				}

				throw new SyntaxException();
			}
			else if(ch == '.') {
				if(isFloat) {
					throw new SyntaxException();
				}
				isFloat = true;
				sb.append((char)next());
			}
			else if(
				(isHex && (ch == 'p' || ch == 'P')) ||
					(!isHex && (ch == 'e' || ch == 'E'))
			) {
				foundExponent = true;
				isFloat = true;
				sb.append((char)next());

				ch = peekNext();

				if(ch == '-' || ch == '+') {
					sb.append((char)next());
					ch = next();
					if(ch < 0 || !Character.isDigit((char)ch)) {
						throw new SyntaxException();
					}
					sb.append(ch);
					ch = peekNext();
				}

				while(ch >= 0 && Character.isDigit((char)ch)) {
					sb.append((char)next());
					ch = peekNext();
				}
			}
			else if(
				Character.isDigit((char)ch) ||
					(isHex && "ABCDEFabcdef".indexOf(ch) >= 0)
			) {
				sb.append((char)next());
			}
			else if(!Character.isLetterOrDigit((char)ch) || SYMBOLS.indexOf((char)ch) >= 0) {
				break;
			}
			else {
				throw new SyntaxException();
			}
		}

		var s = sb.toString();

		if(isFloat) {
			if(s.endsWith("f") || s.endsWith("F")) {
				return new ESExpr.Float32(Float.parseFloat(s));
			}
			else {
				return new ESExpr.Float64(Double.parseDouble(s));
			}
		}
		else {
			if(isHex) {
				if(negative) {
					s = "-" + s.substring(3);
				}
				else {
					s = s.substring(2);
				}
				return new ESExpr.Int(new BigInteger(s, 16));
			}
			else {
				return new ESExpr.Int(new BigInteger(s));
			}
		}
	}

}
