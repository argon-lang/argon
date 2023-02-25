package dev.argon.plugin.loader.js

import org.graalvm.polyglot.Value

final class AsyncIterable[T] private(private val value: Value)

object AsyncIterable {
  given[A]: ValueEncoder[AsyncIterable[A]] with
    override def encode(value: AsyncIterable[A]): Value = value.value
  end given

  given[A]: ValueDecoder[AsyncIterable[A]] with
    override def decode(value: Value): AsyncIterable[A] = AsyncIterable(value)
  end given
}

