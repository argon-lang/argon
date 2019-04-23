package dev.argon

sealed trait CommandParseResult[+A] {

  def map[B](f: A => B): CommandParseResult[B]
  def flatMap[B](f: A => CommandParseResult[B]): CommandParseResult[B]
  def continue[B >: A](next: => CommandParseResult[B]): CommandParseResult[B]

}

object CommandParseResult {
  final case class Value[+A](value: A) extends CommandParseResult[A] {
    override def map[B](f: A => B): CommandParseResult[B] = Value(f(value))
    override def flatMap[B](f: A => CommandParseResult[B]): CommandParseResult[B] = f(value)
    override def continue[B >: A](next: => CommandParseResult[B]): CommandParseResult[B] = this
  }
  case object Skip extends CommandParseResult[Nothing] {
    override def map[B](f: Nothing => B): CommandParseResult[B] = this
    override def flatMap[B](f: Nothing => CommandParseResult[B]): CommandParseResult[B] = this
    override def continue[B >: Nothing](next: => CommandParseResult[B]): CommandParseResult[B] = next
  }
  case object Error extends CommandParseResult[Nothing] {
    override def map[B](f: Nothing => B): CommandParseResult[B] = this
    override def flatMap[B](f: Nothing => CommandParseResult[B]): CommandParseResult[B] = this
    override def continue[B >: Nothing](next: => CommandParseResult[B]): CommandParseResult[B] = this
  }
}