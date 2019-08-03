package dev.argon.compiler.core

sealed trait AbsRef[TContext <: Context with Singleton, T[_ <: Context with Singleton, _[_, _]]] {
  type PayloadSpec[_, _]
  val value: T[TContext, PayloadSpec]
}

object AbsRef {

  def apply[TContext <: Context with Singleton, TPayloadSpec[_, _], T[_ <: Context with Singleton, _[_, _]]](instance: T[TContext, TPayloadSpec]): AbsRef[TContext, T] =
    new AbsRef[TContext, T] {
      override type PayloadSpec[A, B] = TPayloadSpec[A, B]
      override val value: T[TContext, PayloadSpec] = instance

      override def hashCode(): Int = value.hashCode()

      @SuppressWarnings(Array("org.wartremover.warts.Equals"))
      override def equals(o: Any): Boolean = o match {
        case other: AbsRef[_, _] => value.equals(other.value)
        case _ => false
      }

      @SuppressWarnings(Array("org.wartremover.warts.ToString"))
      override def toString: String = value.toString
    }

}
