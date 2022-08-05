package java.nio.file

import java.io.IOException

class NotDirectoryException(message: String) extends IOException(message) {
  def this() = this("")
}
