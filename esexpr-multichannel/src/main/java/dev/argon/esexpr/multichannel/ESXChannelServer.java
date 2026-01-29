package dev.argon.esexpr.multichannel;

import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.ESExprBinaryReader;
import dev.argon.esexpr.ESExprBinaryWriter;
import dev.argon.esexpr.SyntaxException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

public final class ESXChannelServer implements AutoCloseable {

	private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(ESXChannelServer.class);
	
	public ESXChannelServer(InputStream inputStream, OutputStream outputStream) {
		reader = new ESExprBinaryReader(inputStream);
		this.outputStream = outputStream;
		this.writer = new ESExprBinaryWriter(outputStream);
		this.readThread = Thread.startVirtualThread(this::readLoop);
	}

	private boolean isServing = true;
	private final ESExprBinaryReader reader;
	private final OutputStream outputStream;
	private final Thread readThread;
	
	private final Object writeLock = new Object();
	private final ESExprBinaryWriter writer;
	
	private final Queue<ESXChannelServerConnection> connectionQueue = new ArrayDeque<>();
	private final Map<BigInteger, ESXChannelServerConnection> connections = new HashMap<>();

	public synchronized ESXChannelServerConnection accept() throws InterruptedException {
		while(isServing) {
			var connection = connectionQueue.poll();
			if(connection != null) {
				return connection;
			}
			
			wait();
		}
		
		return null;
	}
	
	void send(ClientMessage message) throws IOException {
		synchronized(writeLock) {
			writer.write(ClientMessage.codec().encode(message));
			outputStream.flush();
		}
	}

	private void readLoop() {
		try {
			while(isServing) {
				var expr = this.reader.tryRead();
				if(expr == null) break;
				
				ServerMessage message;
				try {
					message = ServerMessage.codec().decode(expr);
				}
				catch(DecodeException e) {
					log.error("Failed to decode server message", e);
					continue;
				}
				
				synchronized(this) {
					switch(message) {
						case ServerMessage.Connect(var connect) -> {
							var id = connect.id();
							var oldConn = connections.get(id);
							if(oldConn != null) {
								oldConn.onReuseId();
								continue;
							}
							
							var conn = new ESXChannelServerConnection(this, id);
							connections.put(id, conn);
							connectionQueue.add(conn);
							send(new ClientMessage.ConnectAck( new ConnectAck(id)));
							notifyAll();
						}
						case ServerMessage.Disconnect(var disconnect) -> {
							var conn = connections.get(disconnect.id());
							if(conn == null) {
								log.error("Received disconnect for unknown connection ID: {}", disconnect.id());
								continue;
							}
							
							conn.onDisconnect();
						}
						case ServerMessage.DisconnectAck(var disconnectAck) -> {
							var conn = connections.get(disconnectAck.id());
							if(conn == null) {
								log.error("Received disconnect ack for unknown connection ID: {}", disconnectAck.id());
								continue;
							}
							
							conn.onDisconnectAck();
						}
						case ServerMessage.Message(var msg) -> {
							var conn = connections.get(msg.id());
							if(conn == null) {
								log.error("Received message for unknown connection ID: {}", msg.id());
								continue;
							}
							
							conn.onMessage(msg.data());
						}
					}
				}
			}
		}
		catch(IOException | SyntaxException e) {
			log.error("Error reading message", e);
		}
	}

	@Override
	public synchronized void close() throws Exception {
		this.isServing = false;
		notifyAll();
		readThread.interrupt();
	}
}
