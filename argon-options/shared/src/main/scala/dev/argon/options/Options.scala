package dev.argon.options

import scala.annotation.nowarn
import dev.argon.util.given

trait Options[A[_], O <: OptionID] {
  def get(id: O): A[id.ElementType]
  def set(id: O)(value: A[id.ElementType]): Options[A, O]
}

object Options {

  trait OptionValueFunction[A[_], O <: OptionID] {
    def apply[E](id: O with TypedOptionID[E]): A[E]
  }

  def fromFunction[A[_], O <: OptionID](f: OptionValueFunction[A, O]): Options[A, O] = new FunctionBackedOptionOptions(f)

  private class FunctionBackedOptionOptions[A[_], O <: OptionID] private (
    getDefault: OptionValueFunction[A, O],
    map: Map[O, Any],
  ) extends Options[A, O] {

    def this(getDefault: OptionValueFunction[A, O]) = this(getDefault, Map.empty)
    end this

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    final override def get(id: O): A[id.ElementType] =
      map.get(id).map(_.asInstanceOf[A[id.ElementType]]).getOrElse(getDefault(id.asTypedOption))

    final override def set(id: O)(value: A[id.ElementType]): Options[A, O] =
      new FunctionBackedOptionOptions(getDefault, map.updated(id, value))

  }

  private[options] final class MapBackedOptionOptions[A[_], O <: OptionID](map: Map[O, Any])
      extends Options[[X] =>> Option[A[X]], O] {

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def get(id: O): Option[A[id.ElementType]] = map.get(id).map(_.asInstanceOf[A[id.ElementType]])

    override def set(id: O)(value: Option[A[id.ElementType]]): Options[[X] =>> Option[A[X]], O] =
      value match {
        case Some(value) => new MapBackedOptionOptions[A, O](map.updated(id, value))
        case None => new MapBackedOptionOptions[A, O](map.removed(id))
      }

  }

}

trait OptionsConvertFunction[A[_], B[_], F[_]] {
  def apply[X](ax: A[X]): F[B[X]]
}
