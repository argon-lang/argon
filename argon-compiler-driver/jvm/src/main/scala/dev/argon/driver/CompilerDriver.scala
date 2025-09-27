package dev.argon.driver

import dev.argon.backend.BackendProvider
import zio.{ExitCode, FiberFailure, Runtime, Unsafe, ZIO}
import dev.argon.backend.api.BackendFactory
import dev.argon.driver.api.CompilerDriverOptions

import scala.jdk.CollectionConverters.*

class CompilerDriver extends dev.argon.driver.api.CompilerDriver {
  override def runCommand(options: CompilerDriverOptions): Int =
    val runtime = Runtime.default
    
    Unsafe.unsafely {
      runtime.unsafe.run(
        BackendLoaderUtils.loadAllBackends(options.getPluginDirectories.asScala.toSeq)
          .flatMap { backendFactories =>
            CompilerDriverImpl.runCommand(options.getArguments.toSeq)
              .provideLayer(BackendProvider.liveFromFactories(backendFactories))
          }
        
      )
        .getOrElse { cause =>
          throw cause.failureOption
            .map {
              case ex: Throwable => ex
              case _ => FiberFailure(cause)
            }
            .orElse {
              if cause.isInterrupted then
                Some(InterruptedException())
              else
                None
            }
            .orElse {
              cause.defects.headOption
            }
            .getOrElse {
              FiberFailure(cause)
            }
        }
        .code
    }
  end runCommand
}
