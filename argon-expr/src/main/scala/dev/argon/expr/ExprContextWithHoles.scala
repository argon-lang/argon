package dev.argon.expr

import dev.argon.util.{FilePosition, Location, UniqueIdentifier}

trait ExprContextWithHoles extends ExprContext {

  final case class HoleInfo(
    id: UniqueIdentifier,
    holeType: Expr,
    location: Location[FilePosition],
  )derives CanEqual

  override type Hole = HoleInfo
  
}
