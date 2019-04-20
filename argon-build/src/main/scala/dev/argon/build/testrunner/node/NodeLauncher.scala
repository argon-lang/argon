package dev.argon.build.testrunner.node

import dev.argon.build.testrunner.node.ExternalApi.{MethodCallHandler, ServerFunctionCallClient, ServerFunctions}
import com.mi3software.gcrpc.runtime.{BinaryProtocol, RpcConnection, RpcStreamTransport, StandardRpcConnection}
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

trait NodeLauncher {
  def serverFunctions: Task[ServerFunctions]
  def close: UIO[Unit]
}

object NodeLauncher {

  def apply(file: String): UIO[NodeLauncher] = for {
    connOpt <- RefM.make[Option[Fiber[Throwable, (RpcConnection, ServerFunctions)]]](None)
  } yield new NodeLauncher {

    def serverFunctions: Task[ServerFunctions] =
      connOpt
        .modify {
          case conn @ Some(fiber) => (fiber, conn).point[UIO]
          case None =>
            IO.effect {
              val child = new ProcessBuilder("node", "--no-warnings", "--experimental-vm-modules", "--", file)
                .redirectInput(ProcessBuilder.Redirect.PIPE)
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .start()

              val transport = new RpcStreamTransport(child.getInputStream, child.getOutputStream)
              val protocol = new BinaryProtocol()
              val conn = new StandardRpcConnection(transport, protocol, MethodCallHandler)
              conn.startBackground()

              val serverFunctions = ServerFunctionCallClient(conn)

              (conn, serverFunctions)
            }.fork.map(fiber => (fiber, Some(fiber)))
        }
        .flatMap { _.join }
        .map { case (_, serverFuncs) => serverFuncs }

    def close: UIO[Unit] =
      connOpt.modify {
        case Some(fiber) =>
          fiber.await.flatMap {
            case Exit.Success((conn, _)) =>
              IO.effectTotal {
                conn.close()
                ((), None)
              }

            case _ =>
              ((), None).point[UIO]
          }

        case None => ((), None).point[UIO]
      }

  }


}
