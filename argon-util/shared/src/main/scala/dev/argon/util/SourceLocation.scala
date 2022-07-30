package dev.argon.util

final case class FilePosition(line: Int, position: Int) derives CanEqual {

  def compareTo(other: FilePosition): Int =
    if line > other.line then
      1
    else if line < other.line then
      -1
    else if position > other.position then
      1
    else if position < other.position then
      -1
    else
      0

}

object FilePosition:

  given Ordering[FilePosition] with
    def compare(x: FilePosition, y: FilePosition): Int = x.compareTo(y)
  end given

end FilePosition

final case class SourceLocation(fileName: Option[String], start: FilePosition, end: FilePosition)

object SourceLocation {
  def merge(first: SourceLocation, second: SourceLocation): SourceLocation = SourceLocation(first.fileName, first.start, second.end)
}
