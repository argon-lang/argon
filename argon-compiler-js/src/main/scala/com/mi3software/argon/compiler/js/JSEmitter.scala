package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz.NonEmptyList

final class JSEmitter {

  def emitModule(context: JSContext)(module: ArModule[context.type]): context.Comp[JSModule] = {

    val moduleVarName = JSIdentifier("modules")

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
              JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
            )
          ))),

          modulePairs.map { case (refModule, importId) =>
            JSAssignment(
              JSPropertyAccessBracket(moduleVarName, JSString(refModule.descriptor.name)),
              importId
            )
          },

          Vector(
            JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(moduleVarName))
          )

        ).flatten
      )
    )
  }

}
