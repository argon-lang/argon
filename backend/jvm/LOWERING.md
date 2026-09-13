# JVM lowering and ABI

This document specifies the public interface of code produced by the Argon JVM
backend. It is an ABI description for generated-JAR consumers, runtime
implementations, and extern authors. Bytecode-generation strategy and other
compiler internals are outside its scope.

## Generated modular JAR

One Argon tube becomes one modular JAR. Its JPMS module requires `java.base`,
`dev.argon.runtime`, and the module of every referenced tube. Packages that
contain public Argon declarations are exported. Platform metadata may supply
an existing `module-info.class`; the backend preserves it and adds the required
`requires` and `exports` entries.

The default JPMS module name for a tube named `Head.Tail...` is:

```text
argontube<N>.Head.Tail...
```

where `N` is the number of tube-name components and every component uses the
JVM name escaping below. For example, `Argon.Core` becomes
`argontube2.Argon.Core`. The `module-name` platform metadata field overrides
the default.

By default, an Argon module path is appended to the default tube module name to
form its Java package. Platform metadata can override the package independently
for each Argon module. An extern `package-info.class` associates its Java
package with an Argon module by applying the class-retained
`@ArgonModule({"path", "components"})` annotation to that package.

If executable output is requested, the root module's package also contains a
public `Main` class with `public static void main(String[] args)`. It invokes a
root-module Argon function named `main` whose sole parameter is the empty tuple.
Command-line arguments are not passed to the Argon function.

## Public JVM type mapping

| Argon type | JVM type |
| --- | --- |
| `Bool` | `boolean` |
| arbitrary-width `Int` | `java.math.BigInteger` |
| `I8`, `U8` | `byte` |
| `I16`, `U16` | `short` |
| `I32`, `U32` | `int` |
| `I64`, `U64` | `long` |
| `String` | `java.lang.String` |
| `Never` | `dev.argon.runtime.Never` |
| `Array[T]` | `T[]` |
| `Array[T]` when `T` is a type parameter | `java.lang.Object` |
| boxed or type-parameter value | `java.lang.Object` |
| mutable reference | `dev.argon.runtime.RefCell` |
| ordinary function object | `dev.argon.runtime.Function` |
| erased function object | `dev.argon.runtime.FunctionErased` |
| token function object | `dev.argon.runtime.FunctionToken` |
| tuple of 0 through 10 elements | `dev.argon.runtime.Tuple0` through `Tuple10` |
| tuple of more than 10 elements | nested `dev.argon.runtime.TupleXL` values |
| structural reified token | `dev.argon.runtime.TypeInfo` |
| reified instance-value token | the corresponding generated instance value |

Signed and unsigned fixed-width integer types intentionally share JVM carrier
types. Consumers must interpret the bits according to the Argon type. JVM
descriptors and linkage use the erased types in this table.

Record, enum, trait, and instance types map to their generated class or
interface. Conjunction and disjunction types are not currently part of the
supported JVM ABI.

## Calling convention

Generated global functions are public or package-private static methods on the
module's `Globals` class. Generated methods are JVM instance methods; generated
static methods have the `:static:` prefix described below.

Parameters always appear in this order:

1. reified token parameters, in source signature order;
2. ordinary value parameters, in source signature order.

The JVM return type of every Argon function or method is
`dev.argon.runtime.Trampoline`. A caller outside generated Argon code must
resolve the trampoline before treating it as the declared result. The runtime
function interfaces expose the same rule:

```java
interface Function<A, B>       { Trampoline<B> apply(A value); }
interface FunctionErased<B>    { Trampoline<B> apply(); }
interface FunctionToken<A, B>  { Trampoline<B> apply(A token); }
```

These interfaces and the remaining ABI support types are exported by the
`dev.argon.runtime` module.

## Generated declarations

### Functions

Each Argon module containing functions has a final `Globals` class. Its
constructor is private. Each function is a static method with its mangled Argon
name, ABI parameter types, and `Trampoline` return type.

### Records

A record becomes a final class. Reified type tokens are exposed as public final
fields named `:pt0`, `:pt1`, and so on. Argon fields become JVM fields with
encoded names; immutable fields are final.

Construction uses the generated public final nested class `Builder`:

```text
Record.builder(<token arguments>) -> Record.Builder
builder.set_<field>(value)        -> Record.Builder
builder.build()                   -> Record
```

The record also has a public constructor accepting its builder. Builder fields
are public; the fluent setters and `build` provide the normal construction
interface.

### Enums

An enum becomes an abstract sealed class. Every variant is a public static final
nested permitted subclass named with the encoded variant identifier. The enum
stores its reified tokens in public final `:ptN` fields.

Each variant has its own public final nested `Builder`. Call
`Enum.Variant.builder(...)` with the enum token arguments followed by the
variant's token and ordinary arguments, set named fields with `set_<field>`,
then call `build`. Variant arguments are exposed as public final fields named
`:pv0`, `:pv1`, and so on.

### Traits and instances

A trait becomes a JVM interface. Every reified trait token is exposed through
an abstract no-argument method named `:ptN`. Argon trait methods are interface
methods using the normal calling convention.

An Argon instance becomes a final class implementing its trait. Its public
constructor receives token arguments followed by ordinary instance arguments.
Those values are exposed as public final `:ptN` and `:pvN` fields. Instances of
`Argon.Core.Exception.Exception` additionally extend
`dev.argon.runtime.ArgonException`, and their Argon `message` method supplies
Java's `Throwable.getMessage()` result.

## Names and overloads

### JVM name escaping

Argon identifier text is preserved unless it contains a character with special
meaning in JVM names. The escape mapping is:

| Input | Escape |
| --- | --- |
| `/` | `\|` |
| `.` | `\,` |
| `;` | `\?` |
| `$` | `\%` |
| `<` | `\^` |
| `>` | `\_` |
| `[` | `\{` |
| `]` | `\}` |
| `:` | `\!` |

An empty identifier is `\=`. If any character is escaped and the first
character was not itself escaped, `\=` is prepended. A backslash that would
accidentally begin one of these escape sequences is written as `\-`.

Type names `_...`, `package-info`, `module-info`, `Globals`, and `Main` gain an
additional leading underscore. A synthetic nested type is named
`Nested<index>` inside its parent.

Non-named identifier forms use these prefixes:

| Identifier | Encoding |
| --- | --- |
| binary operator | `:b<operator spelling>` |
| unary operator | `:u<operator spelling>` |
| index | `:n` |
| extension | `:x<identifier>` |
| inverse | `:i<identifier>` |
| update | `:m<identifier>` |

### Function and method names

A global function or instance method is named:

```text
<identifier>:a<parameter types>:r<result type>
```

A static method is named:

```text
:static:<identifier>:a<parameter types>:r<result type>
```

A synthetic local function appends `:k<index>` to its parent's function name.
Erased types use this recursive grammar:

| Erased type | Encoding |
| --- | --- |
| scalar builtin `X` | `:bX:a:e` |
| array | `:barray:a<element>:e` |
| function | `:f<input>:r<output>:e` |
| record | `:r<record import>:a<arguments>:e` |
| tuple | `:t<elements>:e` |
| erased | `:_` |

Scalar names are lowercase: `int`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`,
`i64`, `u64`, `bool`, `string`, and `never`.

For example, a function `main` taking and returning the empty tuple is named
`main:a:t:e:r:t:e`.

## Access contract

JVM visibility approximates Argon visibility for ordinary JVM callers, while
the class-retained `dev.argon.runtime.ArgonAccess` annotation records the exact
Argon modifier on generated types, methods, and fields.

| Argon access | JVM access |
| --- | --- |
| top-level `public`, `internal` | `public` |
| top-level `module-private` | package-private |
| member `public`, `internal`, `protected-or-internal` | `public` |
| member `private` | `private` |
| member `protected`, `protected-and-internal` | `protected` |
| member `module-private` | package-private |

Public top-level declarations cause their generated package to be exported
from the JPMS module. JVM visibility alone does not replace Argon access
checking; tooling that needs the exact distinction must read `@ArgonAccess`.

## Type-token contract

`dev.argon.runtime.TypeInfo` implements `dev.argon.runtime.Token`, represents a
concrete JVM class plus zero or more token arguments, and compares
structurally. It carries record, enum, trait, instance-type, function,
reference, tuple, array, builtin, boxed, and type-information tokens. An
instance-value token is represented by the corresponding generated instance
instead. Type parameter tokens are passed explicitly using the JVM type of
their declared kind and are stored or exposed as described above.

## Extern interface

JVM externs are public static JVM methods marked with one of the class-retained
runtime annotations:

- `@ExternFunction("extern-name")`
- `@ExternMethod("extern-name")`
- `@ExternStaticMethod("extern-name")`

The annotation value may be omitted or empty, in which case the JVM method name
is used as the extern name.

The annotated method's JVM descriptor is captured in platform metadata. At a
generated call site the backend invokes exactly that static method, passing
token arguments followed by ordinary arguments. Its parameter and return
types must therefore match the calling convention and type mapping in this
document; in particular, its return type is `Trampoline`.

Extern names must be unique across all supplied classfiles. The command-line
interface accepts individual classfiles and recursively expands directories of
classfiles. Metadata loading reports platform name `jvm` and may also carry:

- an overriding JPMS `module-name`;
- per-Argon-module Java package names;
- additional classfiles, including an existing `module-info.class`, to copy
  into the generated JAR.
