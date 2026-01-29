package dev.argon.esexpr.multichannel;

import dev.argon.esexpr.ESExpr;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Queue;

public final class ESXChannelServerConnection implements AutoCloseable {
	ESXChannelServerConnection(ESXChannelServer channel, BigInteger id) {
		this.channel = channel;
		this.id = id;
	}

	private State state = State.CONNECTED;
	private final ESXChannelServer channel;
	private final BigInteger id;
	
	private Queue<ESExpr> pendingMessages = new ArrayDeque<>();
	
	public synchronized @Nullable ESExpr read() throws InterruptedException {
		while(true) {
			var message = pendingMessages.poll();
			if(message != null) {
				return message;
			}
			
			if(state != State.CONNECTED) {
				return null;
			}

			wait();
		}
	}

	public void write(@NotNull ESExpr msg) throws IOException {
		synchronized(this) {
			if(state != State.CONNECTED) {
				throw new IllegalStateException("Cannot write to a disconnected connection");
			}
		}
		
		channel.send(new ClientMessage.Message(new Message(id, msg)));
	}

	@Override
	public synchronized void close() throws IOException {
		if(state == State.CONNECTED) {
			state = State.DISCONNECTING;
			notifyAll();
			channel.send(new ClientMessage.Disconnect(new Disconnect(id)));
		}
	}

	synchronized void onReuseId() throws IOException {
		switch(state) {
			case CONNECTED -> this.close();
			case DISCONNECTING, DISCONNECTED -> {
				channel.send(new ClientMessage.Disconnect(new Disconnect(id)));
			}
		}
	}

	synchronized void onDisconnect() throws IOException {
		switch(state) {
			case CONNECTED -> {
				state = State.DISCONNECTED;
				notifyAll();
				channel.send(new ClientMessage.DisconnectAck(new DisconnectAck(id)));
			}
			case DISCONNECTING, DISCONNECTED -> {}
		}
	}

	synchronized void onDisconnectAck() {
		if(state == State.DISCONNECTING) {
			state = State.DISCONNECTED;
			notifyAll();
		}
	}

	synchronized void onMessage(@NotNull ESExpr data) {
		pendingMessages.add(data);
		notifyAll();
	}
	
	private enum State {
		CONNECTED,
		DISCONNECTING,
		DISCONNECTED,
	}
}
