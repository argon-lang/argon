package java.io

class FileNotFoundException(message: String) extends IOException(message) {

  @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
  def this() = this(message = null)

}
