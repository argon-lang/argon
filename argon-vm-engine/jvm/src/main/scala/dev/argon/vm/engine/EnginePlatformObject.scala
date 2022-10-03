package dev.argon.vm.engine

import dev.argon.vm.format
import dev.argon.vm.format.{Program, Chunk}
import zio.*

trait EnginePlatformObject {

  def make(program: Program, nativeFunctions: NativeFunctions): IO[VMFormatException, Engine] =
    for
      programWithEntrypoint <-
        ZIO.attempt {
          dev.argon.argonvm.Program.load(
            Program.toJavaProto(program),
            nativeFunctions
          )
        }.refineToOrDie[VMFormatException]


      vm <- ZIO.succeed {
        new dev.argon.argonvm.engine.VM(programWithEntrypoint.program())
      }
    yield new Engine {
      override def execute(): Task[Unit] =
        ZIO.attempt { vm.execute(programWithEntrypoint.entrypoint()) }
    }

}
