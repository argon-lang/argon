package java.nio.file

open class AccessDeniedException(file: String | Null, other: String | Null, reason: String | Null) extends FileSystemException(file, other, reason) {
  def this(file: String | Null) = this(file, null, null)
}
