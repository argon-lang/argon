package java.io

class FileNotFoundException(message: String) extends IOException(message) {
  def this() = this("")
}
