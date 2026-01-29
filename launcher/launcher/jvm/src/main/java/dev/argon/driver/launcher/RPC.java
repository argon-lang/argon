package dev.argon.driver.launcher;

import dev.argon.backend.api.BackendFactory;
import dev.argon.driver.api.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;
import dev.argon.driver.api.command.CommandResult;
import dev.argon.driver.api.command.DriverCommand;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.ESExprCodec;
import dev.argon.esexpr.codecs.StringCodec;
import dev.argon.esexpr.multichannel.ESXChannelServer;
import dev.argon.esexpr.multichannel.ESXChannelServerConnection;
import org.jspecify.annotations.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

final class RPC implements AutoCloseable {
	private RPC(CompilerDriver driver, List<BackendFactory> backendFactories) {
		this.driver = driver;
		this.backendFactories = backendFactories;
	}
	
	private final CompilerDriver driver;
	private final List<BackendFactory> backendFactories;

	private static final Logger log = LoggerFactory.getLogger(RPC.class);

	private final ESXChannelServer server = new ESXChannelServer(System.in, System.out);
	private final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
	private final ESExprCodec<DriverCommand<String, String, String, String>> driverCommandCodec =
		DriverCommand.codec(
			StringCodec.INSTANCE,
			StringCodec.INSTANCE,
			StringCodec.INSTANCE,
			StringCodec.INSTANCE
		);
	
	public static void run(CompilerDriver driver, List<BackendFactory> backendFactories) throws Exception {
		try(var rpc = new RPC(driver, backendFactories)) {
			rpc.listen();
		}
	}

	private void listen() throws InterruptedException {
		while(true) {
			var conn = server.accept();
			if(conn == null) break;
			
			executor.submit(() -> handleConnection(conn));
		}
	}

	private void handleConnection(ESXChannelServerConnection conn) {
		try {
			while(true) {
				var commandExpr = conn.read();
				if(commandExpr == null) break;

				DriverCommand<String, String, String, String> command;
				try {
					command = driverCommandCodec.decode(commandExpr);
				}
				catch(DecodeException e) {
					log.error("Could not parse driver command", e);
					continue;
				}
				
				var liveCommand = ArgonLauncher.realizeCommandPath(command);

				int exitCode;
				try {
					try(var output = new ConnectionOutputStream(conn)) {
						var outputPrintStream = new PrintStream(output, true, StandardCharsets.UTF_8);
						var options = new CompilerDriverOptions(
							backendFactories,
							liveCommand,
							InputStream.nullInputStream(),
							outputPrintStream,
							outputPrintStream
						);

						exitCode = driver.runCommand(options);
					}
				}
				catch(Throwable e) {
					log.error("Error executing command", e);
					exitCode = 1;
				}
				
				var result = new CommandResult.Exit(exitCode);
				conn.write(CommandResult.codec().encode(result));
			}
		}
		catch(InterruptedException _) {}
		catch(IOException e) {
			log.error("Error handling connection", e);
		}
	}

	@Override
	public void close() throws Exception {
		executor.shutdown();
		server.close();
	}
	
	private static final class ConnectionOutputStream extends OutputStream {
		public ConnectionOutputStream(ESXChannelServerConnection conn) {
			this.conn = conn;
		}

		private final ESXChannelServerConnection conn;
		private final CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();
		private final CharBuffer charBuffer = CharBuffer.allocate(1024);

		@Override
		public void write(int b) throws IOException {
			write(new byte[] { (byte)b });
		}

		@Override
		public void write(byte @NonNull [] b, int off, int len) throws IOException {
			ByteBuffer byteBuffer = ByteBuffer.wrap(b, off, len);
			while(byteBuffer.hasRemaining()) {
				charBuffer.clear();
				CoderResult result = decoder.decode(byteBuffer, charBuffer, false);
				charBuffer.flip();
				
				if(charBuffer.hasRemaining()) {
					writeOutput(charBuffer.toString());
				}
				
				if(result.isError()) {
					result.throwException();
				}
			}
		}

		@Override
		public void close() throws IOException {
			charBuffer.clear();
			CoderResult result = decoder.decode(ByteBuffer.allocate(0), charBuffer, true);
			if (result.isError()) {
				result.throwException();
			}
			
			result = decoder.flush(charBuffer);
			if(result.isError()) {
				result.throwException();
			}
			
			charBuffer.flip();
			if(charBuffer.hasRemaining()) {
				writeOutput(charBuffer.toString());
			}
		}
		
		private void writeOutput(String output) throws IOException {
			var message = new CommandResult.Output(output);
			conn.write(CommandResult.codec().encode(message));
		}
		
	}
}
