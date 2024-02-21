package dev.argon.argonvm

trait VMWithTube[R, E] {
  val vm: VM[R, E]
  val tube: vm.Tube
}
