package dev.argon.testrunner;

import dev.argon.driver.api.command.CommandResult;
import dev.argon.driver.api.command.DriverCommand;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.ESExprCodec;
import dev.argon.esexpr.codecs.StringCodec;
import dev.argon.esexpr.multichannel.ESXChannelClient;
import dev.argon.esexpr.multichannel.ESXChannelClientConnection;
import dev.argon.esexpr.multichannel.ServerMessage;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

public final class DriverCommandExecutorRPC implements DriverCommandExecutor {
	public DriverCommandExecutorRPC(RunnerContext context) {
		this.context = context;
	}

	private final RunnerContext context;

	private Process process;
	private ESXChannelClient channel;
	private final List<ESXChannelClientConnection> connections = new ArrayList<>();
	private final Queue<ESXChannelClientConnection> connectionQueue = new ArrayDeque<>();
	
	private final ESExprCodec<DriverCommand<String, String, String, String>> driverCommandCodec =
		DriverCommand.codec(
			StringCodec.INSTANCE,
			StringCodec.INSTANCE,
			StringCodec.INSTANCE,
			StringCodec.INSTANCE
		);
	
	private synchronized ESXChannelClientConnection getConnection() throws Exception {
		if(process == null) {
			var executable = context.distDir().resolve("argon");

			var pb = new ProcessBuilder(executable.toString(), "rpc");

			pb.redirectInput(ProcessBuilder.Redirect.PIPE);
			pb.redirectOutput(ProcessBuilder.Redirect.PIPE);

			process = pb.start();
		}
		
		if(channel == null) {
			channel = new ESXChannelClient(process.getInputStream(), process.getOutputStream());
		}
		
		var conn = connectionQueue.poll();
		if(conn == null) {
			conn = channel.connect();
			connections.add(conn);
		}
		
		return conn;
	}

	@Override
	public CommandExecutionResult execute(
		DriverCommand<String, String, String, String> command
	) throws Exception {		
		var conn = getConnection();
		conn.write(driverCommandCodec.encode(command));
		
		var responseMessage = conn.read();
		if(responseMessage == null) {
			throw new IOException("Connection closed before response");
		}

		StringBuilder output = new StringBuilder();
		while(true) {
			CommandResult response;
			try {
				response = CommandResult.codec().decode(responseMessage);
			}
			catch(DecodeException e) {
				throw new IOException("Failed to decode response message", e);
			}
			
			switch(response) {
				case CommandResult.Exit(var exitCode) -> {
					return new CommandExecutionResult(exitCode, output.toString());
				}
				case CommandResult.Output(var text) -> {
					output.append(text);
				}
			}
		}
	}

	@Override
	public synchronized void close() throws Exception {
		for(var conn : connections) {
			conn.close();
		}
		
		if(channel != null) {
			channel.close();
		}
		
		if(process != null) {
			process.getOutputStream().close();
			process.destroy();
		}
	}
}
