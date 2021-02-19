package dev.argon.armodule.emitter

import zio.{Ref, UIO}


private[emitter] final case class IdentifierState[ID, Elem](idMap: Map[ID, Int], elems: Seq[Elem])
private[emitter] object IdentifierState {
  def initial[ID, Elem]: IdentifierState[ID, Elem] = IdentifierState(Map.empty[ID, Int], Seq.empty[Elem])

  def getIdNum[ID, Elem](ref: Ref[IdentifierState[ID, Elem]])(id: ID, elem: Elem): UIO[Int] =
    ref.modify { idState =>
      idState.idMap.get(id) match {
        case Some(idNum) => (idNum, idState)
        case None =>
          val idNum = idState.elems.size + 1
          (idNum, IdentifierState(idState.idMap.updated(id, idNum), idState.elems :+ elem))
      }
    }
}