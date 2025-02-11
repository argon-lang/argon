package dev.argon.backend.platforms.js;

import org.graalvm.polyglot.*;

public sealed class TubeInput {
    public static non-sealed abstract class Encoded extends TubeInput {
        @HostAccess.Export
        public final String type = "ir-encoded";

        @HostAccess.Export
        public abstract Value data();
    }
}
