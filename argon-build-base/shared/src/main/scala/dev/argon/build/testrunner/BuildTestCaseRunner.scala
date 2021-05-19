package dev.argon.build.testrunner

import cats._
import zio.interop.catz.core._
import dev.argon.backend.Backend
import dev.argon.backend.Backend.{AsFile, AsUnit}
import dev.argon.compiler._
import dev.argon.build.testrunner.BuildTestCaseRunner.EmitDrainCombineFunction
import dev.argon.compiler.options.GeneralOutputOptions
import dev.argon.options.{FileList, OptionID, OptionInfo, Options, OptionsHandler, SingleFile}
import dev.argon.compiler.output.BuildArtifact
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.util.MaybeBlocking

final class BuildTestCaseRunner(protected val backend: Backend, referencePaths: FileList) extends TestCaseRunnerCompilePhase[FileIO with ZipRead with MaybeBlocking] {

  override val name: String = s"Compilation (${backend.name})"

  private def emitDrain[OutputOptionID <: OptionID { type ElementType <: BuildArtifact; type Decoded[_] = SingleFile }](handler: OptionsHandler[OutputOptionID, AsFile])(data: Options[Id, OutputOptionID]): RComp[FileIO, Unit] =
    handler.combineRepr[
      OptionInfo,
      Id,
      AsUnit,
      Comp
    ](handler.optionsToRepr(handler.info), handler.optionsToRepr(data))(
      new EmitDrainCombineFunction[BuildArtifact, OutputOptionID, OptionInfo, Comp] {
        override def baToBuildArtifact(ba: BuildArtifact): BuildArtifact = ba
        override def compToF[A](comp: Comp[A]): Comp[A] = comp
      }
    ).unit

  override def runTest(testCase: TestCase): RComp[FileIO with ZipRead with MaybeBlocking, TestCaseCompletedResult] =
    compileTestCase(testCase, referencePaths).use { buildResult =>
      emitDrain(GeneralOutputOptions.handler)(buildResult.generalOutput) *>
        emitDrain(backend.outputOptions)(buildResult.backendOutput)
    }
      .as(TestCaseActualResult.NotExecuted)

}

object BuildTestCaseRunner {

  // Needed to avoid AbstractMethodError
  abstract class EmitDrainCombineFunction[BA, OutputOptionID <: OptionID { type ElementType <: BA }, A[_], F[_]] extends OptionsHandler.CombineFunction[OutputOptionID, A, Id, AsUnit, F] {

    def baToBuildArtifact(ba: BA): BuildArtifact
    def compToF[X](comp: Comp[X]): F[X]

    override def apply(id: OutputOptionID)(ax: A[id.ElementType], bx: Id[id.ElementType]): F[Unit] =
      compToF(baToBuildArtifact(bx).asStream.runDrain)
  }
}

