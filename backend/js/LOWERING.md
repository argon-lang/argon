# JavaScript lowering and ABI

This document specifies the public interface of code produced by the Argon
JavaScript backend. It is an ABI description for generated-package consumers,
runtime implementations, and extern authors. Details of how the backend builds
JavaScript syntax are outside its scope.

## Generated package

One Argon tube becomes one private ECMAScript package with this shape:

```text
package.json
lib/
  index.js
  <module path>.js
main.js                 # executable tubes only
```

The package has `"type": "module"`. Every Argon module is an entry in the
package's `exports` map. The root Argon module is exported as `.` and written to
`lib/index.js`; a non-root module `A.B` is exported as `./A/B` and written to
`lib/A/B.js` after path-component encoding.

The default package name for a tube named `Head.Tail...` is
`@argon-tube/Head.Tail...`, with every component encoded as described below.
The `package-name` platform metadata field overrides this default. Generated
modules import public declarations from referenced tubes through those package
exports and import the runtime as `@argon-lang/runtime`.

When an executable name is requested, `package.json` maps that name to
`./main.js`. The launcher imports the root module's zero-argument `main`, calls
it, and resolves its trampoline result.

## Public value representation

| Argon value | JavaScript representation |
| --- | --- |
| `Bool` | `boolean` |
| arbitrary-width `Int` | `bigint` |
| `I8`, `U8`, `I16`, `U16`, `I32`, `U32` | `number`, restricted to the corresponding bit width by generated operations |
| `I64`, `U64` | `bigint`, restricted to 64 bits by generated operations |
| `String` | `string` |
| empty tuple | `undefined` |
| non-empty tuple | `Array`, with tuple elements in index order |
| `Array[I8]`, `Array[U8]` | `Int8Array`, `Uint8Array` |
| `Array[I16]`, `Array[U16]` | `Int16Array`, `Uint16Array` |
| `Array[I32]`, `Array[U32]` | `Int32Array`, `Uint32Array` |
| `Array[I64]`, `Array[U64]` | `BigInt64Array`, `BigUint64Array` |
| other arrays | `Array` |
| mutable reference | `RefCell`, whose public `value` property contains the referenced value |
| boxed value | the value itself |

The signed and unsigned fixed-width integer types use the same JavaScript
carrier at a given width. Signedness is applied by the operation being
performed, not by the carrier's JavaScript type.

Argon exceptions are JavaScript exceptions. Implementations of the standard
`Argon.Core.Exception.Exception` trait inherit from `Error` and expose the
Argon `message` method as JavaScript's `message` property.

## Calling convention

Every generated function receives arguments in this order:

1. reified token arguments, in source signature order;
2. ordinary value arguments, in source signature order.

An instance method receives its receiver as JavaScript `this`; its explicit
arguments retain the order above. A static method has no receiver. A normal
function object accepts one value argument, a token function object accepts
one token argument, and an erased function object accepts no arguments.

A call may return either its final value or an opaque delayed value used for
tail calls. Consumers that call generated functions directly must pass the
result to `resolve` from `@argon-lang/runtime`. `resolve` returns non-delayed
values unchanged.

## Declarations and type tokens

Every top-level declaration is a named ESM export. The following exported
shapes form part of the ABI:

- A function export is callable with the calling convention above.
- A record export provides `specialize(...tokens)`. Construct the returned
  class with one object whose keys are encoded field names. The call to
  `specialize` is also used for a record with no token parameters.
- An enum export provides `specialize(...tokens)`. The returned class has a
  `variants` object mapping encoded variant names to variant constructors. The
  call to `specialize` is also used for an enum with no token parameters.
- A trait export is a type constructor used for specialization and dispatch.
- An instance export provides `specialize(...tokens)`. The result provides
  `create(...values)`; parameterless instances are canonicalized.

Record and enum-variant values expose fields as `field_<encoded-name>`.
Immutable fields are non-writable own properties; mutable fields are writable.
Enum-variant constructor arguments are the enum token arguments followed by
the variant's ordinary arguments and, last, an object containing its named
fields. Variant arguments are also exposed as non-writable `args_<index>`
properties.

Specialized record, enum, and trait constructors expose:

- `staticMethods[<mangled method name>]` for static methods;
- `methods[<mangled method name>]`, whose values are symbols used to call
  dynamically dispatched instance methods.

Given a specialized type `T`, a method is invoked as
`value[T.methods[name]](...args)`. Callers must resolve the result as described
above.

Reified tokens have these public representations:

| Argon token | JavaScript representation |
| --- | --- |
| `Bool`, `String` | the strings `"boolean"`, `"string"` |
| integer type | `"bigint"` for `Int`, otherwise the lowercase VM integer name such as `"u32"` |
| array | `new ArrayType(elementToken)` |
| function | `new FunctionType(inputToken, outputToken)` |
| erased function | `new FunctionTypeErased(outputToken)` |
| token function | `new FunctionTypeToken(inputToken, outputToken)` |
| reference | `new RefCellType(innerToken)` |
| tuple | an array of element tokens |
| record, enum, trait, or instance type | its specialized constructor |
| instance value | an instance produced by the specialized instance definition |
| erased type | the runtime singleton `erasedType` |
| type information | the runtime singleton `typeInfo` |
| `Never` | the runtime singleton `neverType` |

Token identity is structural for runtime token classes and tuples, and by
identity for singleton or declaration tokens. Conjunction and disjunction type
tokens are not currently part of the supported JavaScript ABI.

## Names

### Package paths

Tube and module path components use URI component encoding, with `.` encoded
as `%2E`, and then replace every `%` with `$`. For example, `A B` becomes
`A$20B`. A root module is `index.js`. A single-component module named `index`
or `_index` gains one leading underscore in its file name to avoid colliding
with the root module.

### Identifiers

A named Argon identifier is converted to a JavaScript identifier as follows:

1. `$` is escaped as `$$`.
2. Any character not permitted at its position in a JavaScript identifier is
   UTF-8 encoded as one or more lowercase `$hh` bytes.

Other Argon identifier forms use these prefixes:

| Identifier | Encoding |
| --- | --- |
| missing name | `$_` |
| binary operator | `$b<VM operator name>`, with `-` changed to `_` |
| unary operator | `$u<VM operator name>`, with `-` changed to `_` |
| index | `$n` |
| extension | `$x<identifier>` |
| inverse | `$i<identifier>` |
| update | `$m<identifier>` |

### Exported declarations and overloads

A global declaration is exported as:

```text
<identifier>$a<parameter types>$r<result type>
```

A synthetic local declaration appends `$k<index>` to its parent's name.
The erased-type encoding is recursive:

| Erased type | Encoding |
| --- | --- |
| scalar builtin `X` | `$bX$a$e` |
| array | `$barray$a<element>$e` |
| function | `$f<input>$r<output>$e` |
| record | `$r<fully-qualified export>$a<arguments>$e` |
| tuple | `$t<elements>$e` |
| erased | `$_` |

Here scalar names are lowercase: `int`, `i8`, `u8`, `i16`, `u16`, `i32`,
`u32`, `i64`, `u64`, `bool`, `string`, and `never`. A fully-qualified export
starts with encoded tube components separated by `$d`, then `$s`, then every
encoded module component followed by `$s`, and finally the declaration's
export name.

For example, a root-module function `main` taking and returning the empty tuple
is exported as `main$a$t$e$r$t$e`.

## Extern interface

JavaScript extern files are ECMAScript modules containing only import
declarations and top-level calls of these forms:

```js
externFunction("extern-name", function implementation(/* ABI arguments */) {
  // Return an ABI value or delayed value.
});

externMethod("extern-name", implementation);
externStaticMethod("extern-name", implementation);
```

The name is a string literal and the implementation is a JavaScript
expression. Imported bindings referenced by that expression are preserved in
the generated module. Extern names must be unique across all supplied extern
files. The implementation receives and returns values according to this
document's calling and value conventions.

Platform metadata for a JavaScript tube contains the optional `package-name`.
Metadata loading reports the platform name `js` and associates every declared
extern name with its encoded implementation.
