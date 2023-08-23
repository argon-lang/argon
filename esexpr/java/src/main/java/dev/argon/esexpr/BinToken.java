package dev.argon.esexpr;

import java.math.BigInteger;

/*
	Tokens with an integer (last 5 bits are the beginning of the varint)
	000XXXXX constructor
	001XXXXX Int
	010XXXXX NegInt
	011XXXXX String
	100XXXXX Binary
	101XXXXX KWARG
	110XXXXX Reserved
	111XXXXX Fixed


	11100000 Null
	11100001 Constructor End
	11100010 True
	11100011 False
	11100100 Float32
	11100101 Float64
 */
sealed interface BinToken {
	public static record WithInteger(WithIntegerType type, BigInteger value) implements BinToken {
	}

	public static enum WithIntegerType {
		CONSTRUCTOR,
		INT,
		NEG_INT,
		STRING,
		BINARY,
		KWARG,
	}

	public static enum Fixed implements BinToken {
		NULL,
		CONSTRUCTOR_END,
		TRUE,
		FALSE,
		FLOAT32,
		FLOAT64,
	}
}
