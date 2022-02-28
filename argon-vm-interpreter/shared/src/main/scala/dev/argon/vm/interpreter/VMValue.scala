package dev.argon.vm.interpreter

type VMValueNotNull = Byte | Short | Int | Long | Float | Double | GCObjectNotNull | VMCell | VMTuple
type VMValue = VMValueNotNull | Null


