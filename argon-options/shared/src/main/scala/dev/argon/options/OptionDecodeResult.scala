package dev.argon.options

sealed trait OptionDecodeResult[+A] {
  def map[B](f: A => B): OptionDecodeResult[B]
}

object OptionDecodeResult {

  sealed trait Single[+A] extends OptionDecodeResult[A] {
    override def map[B](f: A => B): OptionDecodeResult.Single[B]
  }

  final case class Result[+A](value: A) extends Single[A] {
    override def map[B](f: A => B): Single[B] = Result(f(value))
  }

  case object CouldNotDecode extends Single[Nothing] {
    override def map[B](f: Nothing => B): Single[B] = CouldNotDecode
  }

  case object MultipleValuesNotSupported extends OptionDecodeResult[Nothing] {
    override def map[B](f: Nothing => B): OptionDecodeResult[B] = MultipleValuesNotSupported
  }

}
