package dev.argon.build.testrunner.node

import dev.argon.build.testrunner.node.ExternalApi.{MethodCallHandler, ServerFunctionCallClient, ServerFunctions}
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

  def apply(runtime: Runtime[Any], file: String): UIO[NodeLauncher] = for {
    connOpt <- RefM.make[Option[Fiber[Throwable, (RpcConnection, ServerFunctions)]]](None)
  } yield new NodeLauncher {

    def serverFunctions: RIO[Blocking, ServerFunctions] =
      connOpt
        .modify {
          case conn @ Some(fiber) => IO.succeed((fiber, conn))
          case None =>
            (
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

                serverFunctions = ServerFunctionCallClient(conn)
              } yield (conn, serverFunctions)
            ).fork.map(fiber => (fiber, Some(fiber)))
        }
        .flatMap { _.join }
        .map { case (_, serverFuncs) => serverFuncs }

    def close: ZIO[Blocking, Nothing, Unit] =
      connOpt.modify {
        case Some(fiber) =>
          fiber.await.flatMap {
            case Exit.Success((conn, _)) =>
              conn.close.map { _ => ((), None) }

            case _ =>
              IO.succeed(((), None))
          }

        case None => IO.succeed(((), None))
      }

  }


}
