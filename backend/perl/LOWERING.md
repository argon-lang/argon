# Proposed Perl lowering ABI

> **Status: implemented ABI.** The Rust backend statically parses extern sources
> and emits Perl 5.20-compatible distributions. Perl is not run at compilation
> time.

This document specifies the ABI of distributions produced by the future Argon
Perl backend. Its audience is generated-distribution consumers, runtime
implementors, and extern authors. The target is Perl 5.20 or newer. Generated
code uses conventional packages, coderefs, and blessed references; apart from
the Argon Perl runtime, it may depend only on modules shipped with Perl core.

Only Argon-public declarations are a stable part of this ABI. The spelling and
representation of non-public helpers, closure classes, caches, and other
generated implementation details are explicitly unstable.

## Generated distribution

One Argon tube becomes one installable Perl distribution:

```text
Makefile.PL
META.json
lib/
  Argon/Tube/T2/Argon/Core/Module/_Root.pm
  Argon/Tube/T2/Argon/Core/Module/Text.pm
script/
  example                    # executable tubes only; optional
```

`Makefile.PL` and `META.json` declare Perl 5.20 and the Argon Perl runtime as
requirements. All other runtime requirements must be Perl core modules. Every
generated module is loadable by its package name and returns a true value.

For a tube with `N` name components, the default package of an Argon module is:

```text
Argon::Tube::T<N>::<tube component>...::Module::<module component>...
```

Components use the byte encoding below. The empty Argon module path ends in
the reserved component `_Root`. Thus the root package of `Argon.Core` is:

```perl
Argon::Tube::T2::Argon::Core::Module::_Root
```

The component count prevents a tube/module boundary collision. `_Root` cannot
collide with encoded text because a named underscore is `__` and a byte escape
starts with a hexadecimal digit. The `.pm` path is obtained by replacing `::`
with `/` and appending `.pm`.

Optional Perl platform metadata may contain:

- `distribution-name`, overriding the generated distribution's metadata name;
- `root-package`, overriding `Argon::Tube::T<N>::<tube component>...`.

An overridden root package is a sequence of valid Perl package components and
is used verbatim. It does not change the trailing `Module::_Root` or
`Module::<encoded path>` convention. In the absence of metadata, both the
default namespace and the artifact layout above are required.

## Reversible names and overloads

### Text and identifiers

An encoded non-empty text component is an encoding of the text's canonical
UTF-8 bytes:

- ASCII letters and digits are copied unchanged, except that a digit in the
  first position is written as its `_HH` byte escape;
- an underscore byte is written as `__`;
- every other byte is written as `_HH`, using two uppercase hexadecimal
  digits.

The empty text is the reserved escape `_Q`. A decoder reads an ASCII letter or
digit literally, `__` as an underscore, and `_HH` as one byte. A single
underscore followed by a non-hexadecimal letter is reserved for structural
escapes such as `_Q` and the non-named identifiers below. An unescaped initial
digit is non-canonical because it would not be a valid Perl identifier. The
decoder rejects every other form, as well as invalid or non-canonical UTF-8.
The rules are canonical and reversible while leaving common identifiers
readable. This one encoding is used for package components, file components,
named identifiers, field names, and variant names. For example, `Text` stays
`Text`, `snake_case` is `snake__case`, `2d` is `_32d`, `two words` is
`two_20words`, and `café` is `caf_C3_A9`.

Non-named VM identifiers have the following unambiguous encodings. Nested
`<identifier>` values are encoded recursively.

| Identifier | Encoding |
| --- | --- |
| missing optional name | `_Z` |
| binary operator | `_O_<VM operator name with hyphens changed to underscores>` |
| unary operator | `_U_<VM operator name with hyphens changed to underscores>` |
| index | `_I` |
| extension | `_X_<identifier>` |
| inverse | `_V_<identifier>` |
| update | `_M_<identifier>` |

These forms cannot collide with named identifiers: after a single underscore,
a named encoding always has a hexadecimal digit, while a literal underscore
is doubled.

### Public subroutine names

Public global functions are ordinary fully qualified Perl subroutines in the
module package. A function or method overload has this symbol name:

```text
<identifier>__a<parameter types>__r<result type>
```

The package for a public record, enum, trait, instance type, or instance
declaration is:

```text
<module package>::Type::<mangled declaration name>
```

Here the mangled declaration name uses the same form and its declaration's
erased signature. Values of records and instances are blessed into that
package. An enum variant value is blessed into
`<enum package>::Variant::<encoded variant name>`. These packages are defined
when the containing module's `.pm` file is loaded; they need not have separate
`.pm` files.

Static methods use the same mangled name as their source method; their owner
descriptor supplies the namespace. The erased signature includes ordinary
parameters, not reified token parameters. Its recursive encoding is:

| Erased type | Encoding |
| --- | --- |
| scalar builtin `X` | `b_X_e` |
| array | `b_array_a<element>_e` |
| function | `f_<input>_r<output>_e` |
| record | `r_<fully-qualified declaration>_a<arguments>_e` |
| tuple | `t_<elements>_e` |
| erased | `z_` |

Scalar names are `int`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`,
`u64`, `bool`, `string`, and `never`. Lists are concatenations of
self-delimiting element encodings. A fully qualified declaration is the
length in decimal, an underscore, and then the fully qualified package and
symbol spelling; the length counts UTF-8 bytes. This makes record encodings
reversible even when a root-package override is in effect.

For example, root-module `main` with one empty-tuple parameter and an
empty-tuple result is callable as:

```perl
Argon::Tube::T2::Argon::Core::Module::_Root::main__at__e__rt__e([]);
```

A synthetic local declaration may append `__k<decimal index>` to its parent's
mangled name, but it is not public ABI.

## Public value representation

| Argon value | Perl representation |
| --- | --- |
| `Bool` | an ordinary scalar normalized to `0` or `1` |
| `String` | a defined character string scalar |
| `I8`, `U8`, `I16`, `U16`, `I32`, `U32` | an ordinary integer scalar in the type's range |
| arbitrary-width `Int`, `I64`, `U64` | a normalized `Math::BigInt` object in the type's range |
| empty or non-empty tuple | an array reference, elements in source order |
| `Array[T]` | an array reference, elements in index order |
| mutable reference | an Argon-runtime blessed reference with `get` and `set` methods |
| record or enum variant | a blessed reference of its generated public package |
| trait or instance value | a blessed reference implementing the descriptor dispatch contract |
| boxed value | an Argon-runtime blessed box retaining the contained value |
| function object | a coderef |

`Math::BigInt` is a Perl core module. ABI boundaries must not expose native
scalars for `Int`, `I64`, or `U64`, even where their value happens to fit.
Fixed-width operations enforce their signed or unsigned range. Strings are
decoded Unicode strings rather than UTF-8 byte strings.

Arrays and tuples are distinct at the type-token level despite sharing an
array-reference carrier. A box is always a distinct reference and retains the
identity of a reference-valued payload; boxing and unboxing must neither clone
nor stringify the payload. Record, enum, instance, reference, and box identity
is Perl reference identity.

Mutable references expose:

```perl
$ref->get()
$ref->set($new_value)       # returns [] (the empty tuple)
```

Argon exceptions are Perl exceptions. Generated code and externs throw with
`die $exception`, where `$exception` is the original blessed Argon exception
value. Catching code must preserve that reference. Foreign scalar exceptions
crossing into Argon are wrapped by the runtime's foreign-exception type.

## Calling convention and tail calls

Every callable receives arguments in this order:

1. the receiver, for an instance or dynamically dispatched trait method;
2. reified token arguments, in source signature order;
3. ordinary value arguments, in source signature order.

Global functions and static methods have no receiver. Functions return their
final ABI value directly. There is no public trampoline or delayed-result
type. A normal function object is a coderef accepting one value argument, a
token function object accepts one token argument, and an erased function
object accepts no arguments; all return final values directly.

A direct consumer call therefore needs no runtime resolution step:

```perl
my $result = Argon::Tube::T1::Demo::Module::_Root::id__az___rz_($value);
```

Generated code implements a tail call by evaluating the callee and every
argument before changing `@_`, then using Perl's frame-replacing `goto` form:

```perl
my $next = $callee_coderef;
my @next_args = ($receiver, @token_args, @value_args);
@_ = @next_args;
goto &$next;
```

The callee coderef and arguments must be fully prepared first because `goto
&$next` immediately replaces the current frame. Exceptions propagate through
both direct and tail calls unchanged.

## Type descriptors and tokens

Every public record, enum, trait, instance type, and instance declaration is a
generated package with a class method:

```perl
my $descriptor = Some::Generated::Type->specialize(@tokens);
```

Calling `specialize()` is required even for a declaration with no token
parameters. For the lifetime of a process, repeated specialization of the
same declaration with equal canonical token arguments returns the identical
descriptor reference. Implementations use canonical descriptor objects, not
dynamically generated Perl packages.

A descriptor is a blessed, immutable Argon-runtime object with this public
interface:

| Method | Meaning |
| --- | --- |
| `tokens()` | returns the specialization tokens as a list |
| `construct(@values)` | constructs a record or instance value; unavailable kinds throw |
| `variant($encoded_name)` | returns an enum variant descriptor; unavailable kinds or names throw |
| `static_method($mangled_name)` | returns the static-method coderef or throws |
| `method($mangled_name)` | returns an opaque canonical dispatch key or throws |
| `dispatch($value, $key, @tokens, @values)` | dynamically invokes a trait/record/enum/instance method |

`construct` receives ordinary constructor values only; specialization tokens
are already captured by the descriptor. An instance declaration's descriptor
canonicalizes `construct()` when it has no ordinary instance parameters.
`dispatch` supplies `$value` as the method receiver and follows the standard
receiver/token/value ordering. Descriptor and dispatch-key references are
opaque and compared only by reference identity.

Enum variant descriptors are canonical per specialized enum and encoded
variant name. They expose `construct(@variant_tokens, @variant_values,
$fields)`, where `$fields` is a hash reference keyed by encoded field names.
Record descriptors use `construct($fields)`, with the same field hash shape.
Missing, extra, or duplicate logical fields are errors.

Other reified tokens are immutable, canonical runtime descriptor objects:

| Argon token | Perl token |
| --- | --- |
| builtin integer, `Bool`, `String`, `Never` | runtime builtin descriptor |
| array | runtime array descriptor containing its element token |
| function | runtime descriptor containing input and output tokens |
| erased function | runtime descriptor containing its output token |
| token function | runtime descriptor containing token-kind and output tokens |
| reference | runtime descriptor containing its inner token |
| tuple | runtime descriptor containing its ordered element tokens |
| record, enum, trait, or instance type | generated specialized descriptor |
| instance value | the canonical constructed instance value |
| erased/boxed type | runtime boxed descriptor |
| type information | runtime type-info descriptor |

Structural runtime descriptors are canonicalized recursively. Declaration
descriptors and instance-value tokens use reference identity. Conjunction and
disjunction tokens are not part of this proposed public Perl ABI.

For example, a generic record is specialized and constructed as follows:

```perl
my $pair_type =
    Argon::Tube::T1::Demo::Module::_Root::Type::Pair__at__e__rt__e
    ->specialize($string_t, $i32_t);
my $pair = $pair_type->construct({
    first  => "left",
    second => 7,
});
```

## Observable declarations

Public type declaration packages are stable even though their backing
implementation may inherit from runtime-private classes.

- Record values expose each field through a getter named
  `get_<encoded-name>`. Mutable fields additionally expose
  `set_<encoded-name>($value)`, returning the empty tuple `[]`. Immutable
  fields have no setter.
- Enum values expose `variant()` returning their canonical variant descriptor.
  Variant arguments are returned by `argument($zero_based_index)`. Named
  fields use the same getter/setter convention as records.
- Trait methods are selected with the specialized trait descriptor's
  `method` and `dispatch` operations. Consumers must not infer a Perl method
  name from an Argon trait method.
- Instance values expose their ordinary parameters through
  `argument($zero_based_index)` and are accepted as instance-value tokens.
  Parameterless instances are canonicalized as described above.
- Public methods and static methods are found through the descriptor using
  their mangled overload name. Public global functions alone are installed as
  package subroutines.

Field getters return the stored ABI carrier itself; they do not copy arrays,
objects, or boxes. Generated mutable setters perform the same validation as a
corresponding generated call boundary.

If an executable name is requested, `script/<name>` is an optional generated
launcher. It loads the root package, calls the public root-module `main` whose
sole ordinary parameter and result are the empty tuple, passes `[]`, and uses
the returned `[]` directly. Command-line arguments are not passed to Argon.

## Extern interface

A Perl extern source contains one package and exposes a side-effect-free
manifest function. It is a compilation input, not a runtime dependency:

```perl
sub argon_externs {
    return {
        functions => {
            'argon.io.print' => \&print_impl,
        },
        methods => {
            'argon.example.display' => \&display_impl,
        },
        static_methods => {
            'argon.example.parse' => \&parse_impl,
        },
    };
}
```

The Rust metadata loader parses the complete Perl syntax tree without running
Perl. It accepts package/import and subroutine declarations, `argon_externs`,
and an optional terminal true expression; other top-level executable state is
rejected. Only the literal outer manifest structure is interpreted. Its
mandatory `functions`, `methods`, and `static_methods` hashes may be empty;
their keys are barewords or constant, non-interpolated strings.

Values are preserved Perl expressions. A `\&named_sub` selects that package
subroutine, an anonymous `sub { ... }` is embedded directly, and a
self-contained expression such as `do { ...; sub { ... } }` is preserved.
Imports and selected declarations are stored in platform metadata. Codegen
places each source group in a collision-free private package, evaluates each
expression once, verifies that it yielded a coderef, and caches it. The
original source package and `argon_externs` routine are never copied, loaded,
or executed by a generated distribution.

Extern coderefs follow the same ABI as generated callables: method receivers
come first, followed by token and value arguments; values, integers, direct
returns, and exceptions use the representations in this document. Static
methods and functions have no receiver. An extern must return a final value,
not a trampoline, promise, or wrapper.

Extern names must be unique across every supplied manifest and category. A
duplicate is an error even if both entries contain the same coderef. Unknown
categories, dynamic names, a non-hash manifest, lexical captures, package
variable references, unselected local helpers, and kind mismatches are
compilation errors. Multiple selected targets may call one another or recurse.

## Compatibility boundary

The stable Perl ABI consists only of the generated distribution layout,
public packages and subroutines, public value carriers, descriptors, tokens,
field and variant observations, calling convention, exception behavior, and
extern manifest contract specified above. In particular, this proposal does
not standardize lexical helper names, cache layout, object hash keys, method
resolution algorithms, reference counting, compiler passes, VM instruction
selection, or emitted source formatting.
