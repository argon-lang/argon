package dev.argon.util

import zio.*
import zio.stm.*

sealed trait UniqueIdentifier derives CanEqual

object UniqueIdentifier {
  
  private class UniqueIdImpl extends UniqueIdentifier

  @SuppressWarnings(Array("scalafix:Disable.effect"))
  def make: UIO[UniqueIdentifier] = ZIO.succeed { new UniqueIdImpl }

  
  def makeSTM: USTM[UniqueIdentifier] = STM.succeed { new UniqueIdImpl }
  
}
