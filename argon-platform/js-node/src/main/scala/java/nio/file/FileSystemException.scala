package java.nio.file

import fs2.io.IOException

open class FileSystemException(file: String | Null, other: String | Null, reason: String | Null) extends IOException {
  def this(file: String | Null) = this(file, null, null)
}
