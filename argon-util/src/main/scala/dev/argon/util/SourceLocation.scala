package dev.argon.util

import cats._

final case class FilePosition(line: Int, position: Int) {

  def compareTo(other: FilePosition): Int =
    if(line > other.line)
      1
    else if(line < other.line)
      -1
    else if(position > other.position)
      1
    else if(position < other.position)
      -1
    else
      0

}

object FilePosition {

  implicit val ordering: Order[FilePosition] = Order.from((x, y) => x.compareTo(y))

}

final case class SourceLocation(start: FilePosition, end: FilePosition)

object SourceLocation {
  def merge(first: SourceLocation, second: SourceLocation): SourceLocation =
    SourceLocation(first.start, second.end)

  val empty: SourceLocation = SourceLocation(FilePosition(-1, -1), FilePosition(-1, -1))
}

