package dev.argon.build.testrunner.node

import dev.argon.build.testrunner.node.ExternalApi.{MethodCallHandler, ServerFunctionCallClient, ServerFunctions}
import com.mi3software.gcrpc.runtime.{BinaryProtocol, RpcConnection, RpcStreamTransport, StandardRpcConnection}

final class NodeLauncher(file: String) {

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var connOpt: Option[(RpcConnection, ServerFunctions)] = None

  def serverFunctions: ServerFunctions = connOpt match {
    case Some((_, funcs)) => funcs
    case None =>
      val child = new ProcessBuilder("node", "--no-warnings", "--experimental-vm-modules", "--", file)
        .redirectInput(ProcessBuilder.Redirect.PIPE)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .start()

      val transport = new RpcStreamTransport(child.getInputStream, child.getOutputStream)
      val protocol = new BinaryProtocol()
      val conn = new StandardRpcConnection(transport, protocol, MethodCallHandler)
      conn.startBackground()

      val serverFunctions = ServerFunctionCallClient(conn)

      connOpt = Some((conn, serverFunctions))

      serverFunctions
  }

  def close(): Unit =
    connOpt.foreach { case (conn, _) =>
      conn.close()
      connOpt = None
    }

}
