package dev.argon.vm.engine

import dev.argon.vm.format
import dev.argon.vm.assembler.*
import dev.argon.util.test.{CompileTimeFileSystem, ReadFileCompileTime}
import zio.*
import zio.test.*

object EngineTests extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment & Scope, Any] =
    generateTests("EngineTests", files, None)

  private val files = ReadFileCompileTime.readDirectory("vm/testcases", (isDir, name) => isDir || name.endsWith(".arvm") || name.endsWith(".output.txt"))

  private def generateTests(name: String, fs: CompileTimeFileSystem, dir: Option[Map[String, CompileTimeFileSystem]]): Spec[TestEnvironment & Scope, Any] =
    (fs, dir) match {
      case (CompileTimeFileSystem.Directory(entries), _) =>
        val suites: Chunk[Spec[TestEnvironment & Scope, Any]] = Chunk.fromIterable(entries)
          .map { (name, entry) =>
            generateTests(name, entry, Some(entries))
          }
          .flatMap {
            case Spec(Spec.MultipleCase(multiple)) => multiple
            case spec => Chunk(spec)
          }

        suite(name)(suites*)

      case (CompileTimeFileSystem.File(data), Some(dir)) if name.endsWith(".arvm") =>
        test(name)(
          for
            expectedOutput <- dir.get(name + ".output.txt") match {
              case Some(CompileTimeFileSystem.File(expectedOutput)) => ZIO.succeed(expectedOutput)
              case _ => ZIO.fail ("Could not load expected output file")
            }
            actualOutput <- runProgram(data)
          yield assertTrue(actualOutput == expectedOutput)
        )

      case _ => Spec.empty
    }

  private def runProgram(asm: String): Task[String] =
    for
      program <- Assembler.parse(asm)
      lib <- TestStandardLibrary.make
      engine <- Engine.make(program, lib.toNativeFunctions)
      _ <- engine.execute()
      output <- lib.getOutput
    yield output

}
