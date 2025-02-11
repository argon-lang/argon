package dev.argon.backend.platforms.js

import org.graalvm.polyglot.*;

trait CodegenInput {
    @HostAccess.Export
    def getTubeMapping(): Value // TubeMapping[]

    @HostAccess.Export
    def getTubeInput(): TubeInput

    @HostAccess.Export
    def getExterns(): Value // AsyncIterable<ExternsInfo>
}
