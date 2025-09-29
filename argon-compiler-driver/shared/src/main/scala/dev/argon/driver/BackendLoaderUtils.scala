package dev.argon.driver

import dev.argon.backend.{BackendException, BackendFactory, BackendLoader}
import dev.argon.backend.metadata.BackendSchema
import dev.argon.io.{PathLike, PathUtil, TomlDecodedResource, TomlResource}
import zio.ZIO
import zio.stream.ZStream

import java.io.IOException

private[driver] object BackendLoaderUtils {
  def loadAllBackends(dirs: Seq[PathLike]): ZIO[Any, BackendException | IOException, Seq[BackendFactory]] =
    ZStream.fromIterable(dirs)
      .flatMap(PathUtil.listDirectory)
      .filterZIO(PathUtil.isDirectory)
      .flatMap { dir =>
        ZStream.unwrap(loadBackend(dir).map(ZStream.fromIterable(_)))
      }
      .runCollect
  
  def loadBackend(backendDir: PathLike): ZIO[Any, BackendException | IOException, Option[BackendFactory]] =
    (PathUtil.exists(backendDir) && PathUtil.exists(PathLike.join(backendDir, "backend.toml"))).flatMap {
      case true => 
        loadBackendSchema(backendDir).flatMap { schema =>
          val backendFactory = BackendLoader.load(backendDir, schema)

          ZIO.logWarning(s"Backend does not use a supported api: ${schema.backend.name}")
            .when(backendFactory.isEmpty)
            .as(backendFactory)
        }
        
      case false => ZIO.none
    }
    
  private def loadBackendSchema(backendDir: PathLike): ZIO[Any, BackendException | IOException, BackendSchema] =
    PathUtil.binaryResource(PathLike.join(backendDir, "backend.toml"))
      .decode[[E] =>> TomlDecodedResource[E, BackendSchema]]
      .asDecoded
      .mapError {
        case e: IOException => e
        case e: TomlResource.TomlDecodeError =>
          BackendException(s"Error decoding backend schema toml: ${e.message} at ${e.address}")
      }
      
}
