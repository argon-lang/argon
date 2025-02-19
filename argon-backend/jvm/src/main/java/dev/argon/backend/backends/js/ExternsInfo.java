package dev.argon.backend.backends.js;

import org.graalvm.polyglot.*;

public final class ExternsInfo {
    public ExternsInfo(String sourceCode, String sourceFile) {
        this.sourceCode = sourceCode;
        this.sourceFile = sourceFile;
    }

    @HostAccess.Export
    public final String sourceCode;

    @HostAccess.Export
    public final String sourceFile;
}
