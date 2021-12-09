package dev.argon.compiler.expr

import dev.argon.compiler._
import dev.argon.expr._
import dev.argon.util.UniqueIdentifier

abstract class ArgonExprContext extends ExprContext with UsingContext {
  override type TClass = ArClass
  override type TTrait = ArTrait
  override type TDataConstructor = DataConstructor
  override type TFunction = ArFunc
  override type TMethod = ArMethod
  override type TClassConstructor = ClassConstructor
  override type TVariable = Variable[context.type]
  override type TLocalVariable = LocalVariable[context.type]

  override def classCanEqual: CanEqual[TClass, TClass] = summon[CanEqual[TClass, TClass]]
  override def traitCanEqual: CanEqual[TTrait, TTrait] = summon[CanEqual[TTrait, TTrait]]

  override def dataConstructorCanEqual: CanEqual[TDataConstructor, TDataConstructor] =
    summon[CanEqual[TDataConstructor, TDataConstructor]]

  override def functionCanEqual: CanEqual[TFunction, TFunction] = summon[CanEqual[TFunction, TFunction]]
  override def methodCanEqual: CanEqual[TMethod, TMethod] = summon[CanEqual[TMethod, TMethod]]

  override def classConstructorCanEqual: CanEqual[TClassConstructor, TClassConstructor] =
    summon[CanEqual[TClassConstructor, TClassConstructor]]

  override def variableCanEqual: CanEqual[TVariable, TVariable] = summon[CanEqual[TVariable, TVariable]]

  override def localVariableCanEqual: CanEqual[TLocalVariable, TLocalVariable] =
    summon[CanEqual[TLocalVariable, TLocalVariable]]

}
