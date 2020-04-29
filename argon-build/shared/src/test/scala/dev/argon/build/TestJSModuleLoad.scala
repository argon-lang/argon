package dev.argon.build

import java.io.{FileNotFoundException, IOException}

import dev.argon.build.testrunner.js.JSModuleLoad
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio.{Has, IO, ZLayer}

object TestJSModuleLoad {

  def layer: ZLayer[Has[TestResourceReader.Service] with FileIO, Nothing, JSModuleLoad] = ZLayer.fromFunction { env =>
    val fileIO = env.get[FileIO.Service]
    val res = env.get[TestResourceReader.Service]

    new JSModuleLoad.Service {

      override def loadJSForArgonModule(id: ResourceIndicator): IO[IOException, (String, String)] =
        id match {
          case LibraryResourceIndicator(name) =>
            for {
              id2 <- res.getLibPath(name)
              restPart <- Path.of("js", name + ".js")
              jsPath <- IO.fromEither(id2.parent.map(_.resolve(restPart)).toRight { new FileNotFoundException() })

              content <- fileIO.readAllText(jsPath)
            } yield (name, content)

          case _ => IO.fail(new FileNotFoundException())
        }
    }
  }

}
