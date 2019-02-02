package com.mi3software.argon.compiler.module

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core.{ArModule, GlobalBinding, Namespace}
import com.mi3software.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import com.mi3software.argon.util.stream.{ArStream, StreamTransformation, StreamTransformationM}
import scalapb.GeneratedMessage
import scalaz._
import Scalaz._

final class ModuleEmitter[TComp[+_] : Compilation, TContext <: ModuleContext[TComp] with Singleton](context: TContext) {

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): ArStream[TComp, (String, GeneratedMessage), Unit] = ???

}
