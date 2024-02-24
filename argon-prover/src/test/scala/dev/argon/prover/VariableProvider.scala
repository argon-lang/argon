package dev.argon.prover

import zio.*

trait VariableProvider {
  def nextVariable: UIO[String]
}

object VariableProvider {

  def live: ULayer[VariableProvider] =
    ZLayer(for {
      nextId <- Ref.make(0)
    } yield new VariableProvider {

      override def nextVariable: UIO[String] =
        for {
          num <- nextId.updateAndGet(_ + 1)
        } yield s"#$num"

    })

  def next: URIO[VariableProvider, String] = ZIO.serviceWithZIO[VariableProvider](_.nextVariable)
}