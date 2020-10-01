package dev.argon.compiler.core
import dev.argon.compiler.CompError
import dev.argon.compiler.core.Context.WithRes
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.{ModuleLoad, ResourceReader, SourceParser}
import dev.argon.compiler.loaders.source.SourceModuleCreator
import zio.ZManaged

trait ContextWithModule extends Context {
  override def module[TContext >: ContextWithModule.this.type <: WithRes[ResIndicator] : zio.Tag]: ZManaged[ModuleLoad[ResIndicator, TContext] with ResourceReader[ResIndicator] with SourceParser, CompError, ArModule[ContextWithModule.this.type, DeclarationPayloadSpecifier]] =
    SourceModuleCreator.createModule[ResIndicator, TContext](this)(compilerInput)
}
