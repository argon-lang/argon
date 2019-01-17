package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler.core.Context
import com.mi3software.argon.compiler.types.TypeSystem

object MethodLookup {

  def lookupMethods[TComp[_]](context: Context)(ts: TypeSystem[context.type])(instanceType: ts.SimpleType): TComp[OverloadResult[MemberValue[context.type]]] = ???

}
