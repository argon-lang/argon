package java.io

open class IOException(message: String, cause: Throwable | Null) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this("", cause)
  def this() = this("")
}
