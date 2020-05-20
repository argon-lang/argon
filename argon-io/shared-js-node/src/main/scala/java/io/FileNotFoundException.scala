package java.io

class FileNotFoundException(message: String) extends IOException(message) {

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def this() = this(message = null)

}
