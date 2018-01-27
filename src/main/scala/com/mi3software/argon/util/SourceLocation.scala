package com.mi3software.argon.util

final case class FilePosition(line: Int, position: Int)
final case class SourceLocation(start: FilePosition, end: FilePosition)

object SourceLocation {
  def merge(first: SourceLocation, second: SourceLocation): SourceLocation =
    SourceLocation(first.start, second.end)

  val empty: SourceLocation = SourceLocation(FilePosition(-1, -1), FilePosition(-1, -1))
}

