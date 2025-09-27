package dev.argon.backend.jsApi

import dev.argon.backend.metadata.{BackendLoaderOptions, BackendMetadata}
import dev.argon.backend.{Backend, BackendException, BackendFactory, JavaApiBackendLoader}
import zio.{Scope, ZIO}

import java.io.IOException
import java.nio.file.Path

private[backend] final class JSApiBackendFactory(
  backendDir: Path,
  override val metadata: BackendMetadata,
  loaderOptions: BackendLoaderOptions.JSLoaderOptions,
) extends BackendFactory {
  override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
    ZIO.fromAutoCloseable(ZIO.attempt {
      val loadedDir = loadDirectory(backendDir)
      val processedDir = ImportResolver.resolveModules(loaderOptions.`import-path`, loadedDir)
      
      JSApiBackendLoaderImpl(
        processedDir,
        loaderOptions.`import-path`,
        loaderOptions.`export-name`,
      )
    }.refineToOrDie[IOException])
      .flatMap { backendLoader =>
        ZIO.succeed(backendLoader.getBackendFactory)
      }
      .flatMap(JavaApiBackendLoader.loadJavaApiBackend(metadata.backend.name))
    
    
  
  private def loadDirectory(dir: Path): java.util.Map[String, String] =
    val files = java.util.HashMap[String, String]()

    java.nio.file.Files.walk(dir)
      .filter(java.nio.file.Files.isRegularFile(_))
      .filter { path =>
        val name = path.getFileName.toString
        name.endsWith(".js") || name.endsWith(".mjs") || name == "package.json"
      }
      .forEach { path =>
        val relativePath = dir.relativize(path).toString
        val content = java.nio.file.Files.readString(path)
        files.put(relativePath, content)
      }

    files
  end loadDirectory
}
