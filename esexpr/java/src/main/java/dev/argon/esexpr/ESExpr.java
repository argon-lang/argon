package dev.argon.esexpr;

import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;
import java.util.List;
import java.util.Map;

public sealed interface ESExpr {
    public static record Constructed(@NotNull String constructor, @NotNull Map<String, ? extends ESExpr> kwargs, @NotNull List<? extends @NotNull ESExpr> arguments) implements ESExpr {}

    public static record Bool(boolean b) implements ESExpr {}

    public static record Int(@NotNull BigInteger n) implements ESExpr {}

    public static record Str(@NotNull String s) implements ESExpr {}

    public static record Binary(byte @NotNull[] b) implements ESExpr {}

    public static record Float32(float f) implements ESExpr {}

    public static record Float64(double d) implements ESExpr {}


    public static record Null() implements ESExpr {}

}
