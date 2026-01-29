
import {ArrayStringPool, ExprReader, writeExpr} from "@argon-lang/esexpr/binary_format";
import type {ESExpr} from "@argon-lang/esexpr";
import {Monitor} from "./monitor.js";
import {ClientMessage, ServerMessage} from "./framing.js";

export interface ESXChannelServer {
    accept(): Promise<ESXChannelServerConnection | undefined>;
    stop(): Promise<void>;
}

export namespace ESXChannelServer {
    export function create(input: AsyncIterable<Uint8Array>, output: (data: Uint8Array) => Promise<void>): ESXChannelServer {
        return new ESXChannelServerImpl(input, output);
    }
}

class ESXChannelServerImpl implements ESXChannelServer {
    constructor(input: AsyncIterable<Uint8Array>, output: (data: Uint8Array) => Promise<void>) {
        this.#reader = new ExprReader(input);
        this.#output = output;
        this.#readThread = this.#readLoop();
        this.#writeThread = this.#writeLoop();
    }

    #isServing = true;
    readonly #reader: ExprReader;
    readonly #output: (data: Uint8Array) => Promise<void>;

    readonly #connections = new Map<bigint, ESXChannelServerConnectionImpl>();

    readonly #connectionQueue: ESXChannelServerConnection[] = [];
    readonly #readThread: Promise<void>;
    readonly #writeQueue: ClientMessage[] = [];
    readonly #writeThread: Promise<void>;

    readonly #monitor = new Monitor();

    accept(): Promise<ESXChannelServerConnection | undefined> {
        return this.#monitor.runExclusive(async () => {
            while(this.#isServing) {
                const connection = this.#connectionQueue.shift();
                if(connection !== undefined) {
                    return connection;
                }

                await this.#monitor.wait();
            }

            return undefined;
        });
    }


    send(message: ClientMessage) {
        this.#writeQueue.push(message);
        return this.#monitor.runExclusive(async () => {
            this.#monitor.notifyAll();
        });
    }

    #readLoop(): Promise<void> {
        return this.#monitor.runExclusive(async () => {
            try {
                while(this.#isServing) {
                    const expr = await this.#reader.tryReadExpr();
                    if(expr === undefined) {
                        this.#isServing = false;
                        break;
                    }

                    const messageRes = ServerMessage.codec.decode(expr);
                    if(!messageRes.success) {
                        console.error("Failed to decode message from client", messageRes.message);
                        continue;
                    }

                    console.error("Decoded message from client", messageRes.value);

                    const message = messageRes.value;
                    switch(message.$type) {
                        case "connect":
                        {
                            const id = message.connect.id;
                            const oldConnection = this.#connections.get(id);
                            if(oldConnection !== undefined) {
                                oldConnection.onReusedId();
                                continue;
                            }


                            const connection = new ESXChannelServerConnectionImpl(this, id);
                            this.#connections.set(id, connection);
                            this.#connectionQueue.push(connection);
                            this.#writeQueue.push({
                                $type: "connect-ack",
                                connectAck: { id },
                            });
                            this.#monitor.notifyAll();
                            break;
                        }

                        case "disconnect":
                        {
                            const connection = this.#connections.get(message.disconnect.id);
                            if(connection === undefined) {
                                console.error("Received disconnect message for unknown connection", message.disconnect.id);
                                continue;
                            }

                            connection.onDisconnect();
                            break;
                        }

                        case "disconnect-ack":
                        {
                            const connection = this.#connections.get(message.disconnectAck.id);
                            if(connection === undefined) {
                                console.error("Received disconnect message for unknown connection", message.disconnectAck.id);
                                continue;
                            }

                            connection.onDisconnectAck();
                            break;
                        }

                        case "message":
                        {
                            const connection = this.#connections.get(message.message.id);
                            if(connection === undefined) {
                                console.error("Received message for unknown connection", message.message.id);
                                continue;
                            }

                            connection.onMessage(message.message.data);
                        }
                    }
                }
            }
            catch(e) {
                console.error("Error reading channel input", e);
            }
        })
    }


    #writeLoop(): Promise<void> {
        return this.#monitor.runExclusive(async () => {
            try {
                while(this.#isServing) {
                    const message = this.#writeQueue.shift();
                    if(message === undefined) {
                        await this.#monitor.wait();
                        continue;
                    }

                    console.error("Sending message to client", message);

                    const expr = ClientMessage.codec.encode(message);

                    for await (const chunk of writeExpr(expr, new ArrayStringPool())) {
                        await this.#output(chunk);
                    }
                }
            }
            catch(e) {
                console.error("Error writing channel output", e);
                this.#isServing = false;
            }
        });
    }

    stop(): Promise<void> {
        return this.#monitor.runExclusive(async () => {
            this.#isServing = false;
            this.#monitor.notifyAll();
            await this.#readThread;
            await this.#writeThread;
        });
    }
}

export interface ESXChannelServerConnection {
    read(): Promise<ESExpr | undefined>;
    write(data: ESExpr): Promise<void>;
    close(): Promise<void>;
}

class ESXChannelServerConnectionImpl implements ESXChannelServerConnection {
    constructor(server: ESXChannelServerImpl, id: bigint) {
        this.#server = server;
        this.#id = id;
    }

    #monitor = new Monitor();
    #state: "connected" | "disconnecting" | "disconnected" = "connected";
    #server: ESXChannelServerImpl;
    #id: bigint;

    #pendingMessages: ESExpr[] = [];

    read(): Promise<ESExpr | undefined> {
        return this.#monitor.runExclusive(async () => {
            while(true) {
                const message = this.#pendingMessages.shift();
                if(message !== undefined) {
                    return message;
                }

                if(this.#state === "connected") {
                    await this.#monitor.wait();
                }
                else {
                    return undefined;
                }
            }
        });
    }

    async write(data: ESExpr): Promise<void> {
        if(this.#state !== "connected") {
            throw new Error("Cannot write to disconnected connection");
        }

        await this.#server.send({
            $type: "message",
            message: {
                id: this.#id,
                data
            },
        });
    }

    close(): Promise<void> {
        return this.#monitor.runExclusive(async () => {
            if(this.#state === "connected") {
                this.#state = "disconnecting";
                this.#monitor.notifyAll();
                await this.#server.send({
                    $type: "disconnect",
                    disconnect: { id: this.#id },
                });
            }
        });
    }



    async onReusedId(): Promise<void> {
        switch(this.#state) {
            case "connected":
                await this.close();
                break;

            case "disconnecting":
            case "disconnected":
                await this.#server.send({
                    $type: "disconnect",
                    disconnect: { id: this.#id },
                });
                break;
        }
    }

    onDisconnect(): Promise<void> {
        return this.#monitor.runExclusive(async () => {
            switch(this.#state) {
                case "connected":
                    this.#state = "disconnected";
                    this.#monitor.notifyAll();
                    await this.#server.send({
                        $type: "disconnect-ack",
                        disconnectAck: { id: this.#id },
                    })
                    break;

                case "disconnecting":
                case "disconnected":
                    break;
            }
        });
    }

    onDisconnectAck() {
        return this.#monitor.runExclusive(async () => {
            if(this.#state === "disconnecting") {
                this.#state = "disconnected";
                this.#monitor.notifyAll();
            }
        });
    }

    onMessage(data: ESExpr) {
        return this.#monitor.runExclusive(async () => {
            this.#pendingMessages.push(data);
            this.#monitor.notifyAll();
        })
    }
}
