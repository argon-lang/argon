package dev.argon.backend.backends.js

import org.graalvm.polyglot.*

trait StreamPull {
  @HostAccess.Export
  def pull(): Value // Promise<A[] | null>

  @HostAccess.Export
  def close(): Value // Promise<void>
}
