package dev.argon.esexpr.multichannel;

import dev.argon.esexpr.ESExpr;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Queue;

public class ESXChannelClientConnection implements AutoCloseable {
	ESXChannelClientConnection(ESXChannelClient channel, BigInteger id) {
		this.channel = channel;
		this.id = id;
	}

	private final ESXChannelClient channel;
	private final BigInteger id;
	private State state = State.CONNECTING;
	private final Queue<ESExpr> pendingMessages = new ArrayDeque<>();

	public synchronized @Nullable ESExpr read() throws InterruptedException {
		while(true) {
			ESExpr msg = pendingMessages.poll();
			if(msg != null) {
				return msg;
			}

			if(state == State.CONNECTED) {
				wait();
			}
			else {
				return null;
			}
		}
	}

	public synchronized void write(ESExpr msg) throws InterruptedException, IOException {
		if(state != State.CONNECTED) {
			throw new IOException("Not connected");
		}

		channel.send(new ServerMessage.Message(new Message(id, msg)));
	}

	synchronized void connect() throws IOException, InterruptedException {
		channel.send(new ServerMessage.Connect(new Connect(id)));

		while(state == State.CONNECTING) {
			wait();
		}
	}

	synchronized void connectAck() {
		if(state == State.CONNECTING) {
			state = State.CONNECTED;
			notifyAll();
		}
	}

	synchronized void disconnect() throws IOException {
		if(state == State.DISCONNECTING || state == State.DISCONNECTED) {
			state = State.DISCONNECTED;
			notifyAll();
		}
		else {
			state = State.DISCONNECTING;
			channel.send(new ServerMessage.DisconnectAck(new DisconnectAck(id)));
			notifyAll();
		}
	}

	synchronized void disconnectAck() {
		state = State.DISCONNECTED;
		notifyAll();
	}

	synchronized void message(@NotNull ESExpr data) {
		if(state == State.CONNECTED) {
			pendingMessages.add(data);
			notifyAll();
		}
	}

	@Override
	public synchronized void close() throws Exception {
		if(state != State.DISCONNECTED && state != State.DISCONNECTING) {
			state = State.DISCONNECTING;
			channel.send(new ServerMessage.Disconnect(new Disconnect(id)));
			notifyAll();
		}
	}

	private enum State {
		CONNECTING,
		CONNECTED,
		DISCONNECTING,
		DISCONNECTED,
	}
}
