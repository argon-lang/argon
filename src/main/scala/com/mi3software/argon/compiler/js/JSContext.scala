package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._

class JSContext extends Context {
  override val typeSystem: ArgonTypeSystem[this.type] = new ArgonTypeSystem[this.type]
}
