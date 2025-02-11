package dev.argon.backend

open class BackendException(message: String | Null = null, cause: Throwable | Null = null) extends Exception(message, cause)
