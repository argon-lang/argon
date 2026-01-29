package dev.argon.esexpr.multichannel;

import dev.argon.esexpr.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ESXChannelClient implements AutoCloseable {
	public ESXChannelClient(InputStream inputStream, OutputStream outputStream) {
		this.inputStream = inputStream;
		this.outputStream = outputStream;
		this.writer = new ESExprBinaryWriter(outputStream);
		
		readThread = Thread.startVirtualThread(this::readLoop);
	}
	
	private static final Logger log = LoggerFactory.getLogger(ESXChannelClient.class);

	private final InputStream inputStream;
	private final OutputStream outputStream;
	private final Thread readThread;
	private final ESExprBinaryWriter writer;
	private final ESExprCodec<ServerMessage> serverMessageCodec = ServerMessage.codec();
	
	private final Map<BigInteger, ESXChannelClientConnection> connections = new HashMap<>();
	private final Set<BigInteger> reusableIds = new HashSet<>();
	private BigInteger nextId = BigInteger.ZERO;
	
	
	
	public ESXChannelClientConnection connect() throws IOException, InterruptedException {
		ESXChannelClientConnection conn;
		synchronized(this)
		{
			var id = choseNextId();
			conn = new ESXChannelClientConnection(this, id);
			connections.put(id, conn);
		}
		
		conn.connect();
		return conn;
	}
	
	private BigInteger choseNextId() {
		var iter = reusableIds.iterator();
		if(iter.hasNext()) {
			var id = iter.next();
			iter.remove();
			return id;
		}
		else {
			var id = nextId;
			nextId = nextId.add(BigInteger.ONE);
			return id;
		}
	}

	private void readLoop() {
		var reader = new ESExprBinaryReader(List.of(), inputStream);
		var codec = ClientMessage.codec();
		
		while(!Thread.interrupted()) {
			try {
				var messageExpr = reader.tryRead();
				if(messageExpr == null) break;
				
				ClientMessage message = codec.decode(messageExpr);
				
				switch(message) {
					case ClientMessage.ConnectAck(var connectAck) -> {
						var conn = getConnection(connectAck.id());
						conn.connectAck();
					}
					case ClientMessage.Disconnect(var disconnect) -> {
						var conn = getConnection(disconnect.id());
						conn.disconnect();
					}
					case ClientMessage.DisconnectAck(var disconnectAck) -> {
						var conn = getConnection(disconnectAck.id());
						conn.disconnectAck();
					}
					case ClientMessage.Message(var msg) -> {
						var conn = getConnection(msg.id());
						conn.message(msg.data());
					}
				}
				
			}
			catch(InterruptedIOException e) {
				break;
			}
			catch(IOException e) {
				if(!Thread.interrupted()) {
					log.error("Error reading message", e);
				}
				break;
			}
			catch(SyntaxException e) {
				log.error("Error reading message", e);
				break;
			}
			catch(DecodeException | MessageProcessingException e) {
				log.error("Error processing message", e);
			}
		}
	}
	
	private synchronized ESXChannelClientConnection getConnection(BigInteger id) throws MessageProcessingException {
		var conn = connections.get(id);
		if(conn == null) {
			throw new MessageProcessingException("Unknown connection ID: " + id);
		}
		
		return conn;
	}
	
	private synchronized void releaseConnection(BigInteger id) {
		reusableIds.add(id);
		connections.remove(id);
	}
	
	@Override
	public void close() {
		readThread.interrupt();
	}

	public synchronized void send(ServerMessage message) throws IOException {
		writer.write(serverMessageCodec.encode(message));
		outputStream.flush();
	}
}
