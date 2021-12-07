package dev.argon.expr

import dev.argon.util.UniqueIdentifier

object TestExprContext extends ExprContext {
  override type TClass = String
  override type TTrait = String
  override type TDataConstructor = String
  override type TFunction = String
  override type TMethod = String
  override type TClassConstructor = String
  override type TVariable = String
  override type TLocalVariable = String
  override type THole = UniqueIdentifier

  private val stringCanEqual: CanEqual[String, String] = summon[CanEqual[String, String]]

  override def classCanEqual: CanEqual[TClass, TClass] = stringCanEqual
  override def traitCanEqual: CanEqual[TTrait, TTrait] = stringCanEqual
  override def dataConstructorCanEqual: CanEqual[TDataConstructor, TDataConstructor] = stringCanEqual
  override def functionCanEqual: CanEqual[TFunction, TFunction] = stringCanEqual
  override def methodCanEqual: CanEqual[TMethod, TMethod] = stringCanEqual
  override def classConstructorCanEqual: CanEqual[TClassConstructor, TClassConstructor] = stringCanEqual
  override def variableCanEqual: CanEqual[TVariable, TVariable] = stringCanEqual
  override def localVariableCanEqual: CanEqual[TLocalVariable, TLocalVariable] = stringCanEqual

  override def holeCanEqual: CanEqual[THole, THole] = summon[CanEqual[THole, THole]]
}
