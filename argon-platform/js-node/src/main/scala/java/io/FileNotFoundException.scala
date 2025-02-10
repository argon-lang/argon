package java.io

open class FileNotFoundException(message: String | Null) extends IOException {
  def this() = this(null)
}
