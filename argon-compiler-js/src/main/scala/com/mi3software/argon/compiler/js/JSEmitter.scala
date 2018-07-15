package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz.NonEmptyList

final class JSEmitter {

  private val moduleVarName = JSIdentifier("modules")

  def emitModule(context: JSContext)(module: ArModule[context.type]): context.Comp[JSModule] = {

    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_$i")) }


    context.compMonadInstance.point(
      JSModule(
        Vector(
          modulePairs.map { case (refModule, importId) =>
            JSImportAllStatement(None, importId, refModule.descriptor.name)
          },

          Vector(JSConst(NonEmptyList(
            JSBindValue(moduleVarName,
              JSObjectLiteral(
                modulePairs.map { case (refModule, importId) =>
                  JSObjectProperty(refModule.descriptor.name, importId)
                }
              )
            )
          )))
        ).flatten
      )
    )
  }



}
