package dev.argon.build.testrunner.js

import dev.argon.build.testrunner.js.ExternalApi.{MethodCallHandler, ServerFunctionCallClient, ServerFunctions}
import com.mi3software.identityrpc.runtime.{BinaryProtocol, RpcConnection, RpcStreamTransport, StandardRpcConnection}
import cats._
import cats.instances._
import zio._
import zio.blocking.Blocking
import zio.interop.catz._

trait NodeLauncher {
  def serverFunctions: RIO[Blocking, ServerFunctions]
  def close: ZIO[Blocking, Nothing, Unit]
}

object NodeLauncher {

  def apply(file: String): ZManaged[Blocking, Throwable, NodeLauncher] =
    ZManaged.make[Blocking, Blocking, Throwable, NodeLauncher](
      for {
        runtime <- ZIO.runtime[Any]

        conn <-
          for {
            child <- IO.effect {
              new ProcessBuilder("node", "--no-warnings", "--experimental-vm-modules", "--", file)
                .redirectInput(ProcessBuilder.Redirect.PIPE)
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .start()
            }

            transport <- RpcStreamTransport(child.getInputStream, child.getOutputStream)
            protocol = new BinaryProtocol()
            conn <- StandardRpcConnection(runtime, transport, protocol, MethodCallHandler)
            _ <- conn.startBackground
          } yield conn


      } yield new NodeLauncher {

        def serverFunctions: RIO[Blocking, ServerFunctions] =
          IO.succeed(ServerFunctionCallClient(conn))

        def close: ZIO[Blocking, Nothing, Unit] =
          conn.close

      }
    )(_.close)


}
