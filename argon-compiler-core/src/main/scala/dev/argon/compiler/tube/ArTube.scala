package dev.argon.compiler.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.module.*

trait ArTubeC extends UsingContext with DeclarationMode {
  val tubeName: TubeName
  val options: context.Options

  def module(path: ModulePath): Comp[ArModule & HasDeclaration[IsDeclaration]]
  def modulePaths: Set[ModulePath]

  def asDeclaration: Option[this.type & HasDeclaration[true]]
}
