package dev.argon.expr

import scala.collection.mutable

object VariableLinearizer {
  
  def apply(exprContext: ExprContext { type Hole = Nothing })(vars: Set[exprContext.Var]): Seq[exprContext.Var] =
    import exprContext.*

    val orderedVars = mutable.ArrayBuffer[Var]()
    orderedVars.sizeHint(vars.size)

    val seenVars = mutable.Set[Var]()

    def dfs(v: Var): Unit =
      if vars.contains(v) && seenVars.add(v) then
        val referencedVars = FreeVariableScanner(exprContext)(v.varType)
        referencedVars.foreach(dfs)
        orderedVars.append(v)
      end if
    end dfs

    vars.foreach(dfs)

    orderedVars.toSeq
  end apply

}
