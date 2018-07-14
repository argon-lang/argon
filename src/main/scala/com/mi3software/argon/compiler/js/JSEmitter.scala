package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._

final class JSEmitter {

  def emitModule(context: JSContext)(module: ArModule[context.type]): context.Comp[JSModule] =
    context.compMonadInstance.point(
      JSModule(
        Vector(
          module.referencedModules
            .zipWithIndex
            .map { case (refModule, i) => JSImportAllStatement(None, JSIdentifier(s"module_$i"), refModule.descriptor.name) }
        ).flatten
      )
    )



}
