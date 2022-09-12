package dev.argon.compiler.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.module.*

trait ArTubeC extends UsingContext with DeclarationMode {
  val tubeName: TubeName
  val options: IsImplementation match {
    case true => context.Options
    case false => Unit
  }

  def asDeclaration: Option[this.type & HasImplementation[true]]

  def module(path: ModulePath): Comp[ArModule & HasImplementation[IsImplementation]]
  def modulePaths: Set[ModulePath]
}
