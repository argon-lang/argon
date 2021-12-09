package dev.argon.parser.impl

final case class WithLength[+A](value: A, length: Int, lengthAfterNewLine: Boolean) {

  def combine[B, C](len2: WithLength[B])(f: (A, B) => C): WithLength[C] =
    WithLength(
      f(value, len2.value),
      if(len2.lengthAfterNewLine) len2.length else length + len2.length,
      lengthAfterNewLine || len2.lengthAfterNewLine,
    )

}
