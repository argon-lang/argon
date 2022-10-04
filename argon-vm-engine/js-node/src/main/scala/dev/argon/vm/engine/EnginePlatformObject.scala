package dev.argon.vm.engine

import dev.argon.vm.format
import dev.argon.vm.format.{Program, Chunk}
import zio.*

import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.*

import typings.argonLangVmEngine.mod.VM
import typings.argonLangVmFormat.{programMod, argonvmMod as protoMod}

trait EnginePlatformObject {
  def make(program: Program, nativeFunctions: NativeFunctions): IO[VMFormatException, Engine] =
    for
      programWithEntrypoint <-
        ZIO.attempt {
          val protoProgram = new Uint8Array(program.toByteArray.toTypedArray.buffer)
          val jsProtoProgram = protoMod.Program.^.fromBinary(protoProgram)

          programMod.loadProgram(jsProtoProgram, nativeFunctions)
        }.catchAll {
          case jsErr @ JavaScriptException(ex) =>
            ex.asInstanceOf[Matchable] match {
              case ex: js.Error => ZIO.fail(VMException(ex))
              case _ => ZIO.die(jsErr)
            }

          case ex => ZIO.die(ex)
        }


      vm <- ZIO.succeed {
        new typings.argonLangVmEngine.mod.VM(programWithEntrypoint._1)
      }
    yield new Engine {
      override def execute(): Task[Unit] =
        ZIO.fromPromiseJS(vm.execute(programWithEntrypoint._2))
    }

}
