package dev.argon.backend

import dev.argon.compiler.Context
import java.io.IOException

type BackendContext = Context { type Error >: BackendException | IOException }
