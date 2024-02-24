package dev.argon.util

import cats.Order

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

  given Order[FilePosition] with
    def compare(x: FilePosition, y: FilePosition): Int = x.compareTo(y)
  end given

end FilePosition

final case class FileOffset(offset: Int) derives CanEqual {
  def compareTo(other: FileOffset): Int =
    offset.compareTo(other.offset)
}

object FileOffset:
  given Ordering[FileOffset] with
    def compare(x: FileOffset, y: FileOffset): Int = x.compareTo(y)
  end given

  given Order[FileOffset] with
    def compare(x: FileOffset, y: FileOffset): Int = x.compareTo(y)
  end given
end FileOffset


final case class Location[+Pos](fileName: Option[String], start: Pos, end: Pos)

object Location {
  def merge[Pos](first: Location[Pos], second: Location[Pos]): Location[Pos] = Location(first.fileName, first.start, second.end)
}

type SourceLocation = Location[FilePosition]
