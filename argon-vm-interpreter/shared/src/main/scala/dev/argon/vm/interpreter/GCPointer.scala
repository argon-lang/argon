package dev.argon.vm.interpreter

enum GCPointer {
  case LocalVariable(cell: VMCell)
}

