package dev.argon.vm.format

import java.io.EOFException
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream


object FormatUtil {
  def writeIndex(os: OutputStream, index: Long): Unit =
    var index2 = index
    while
      var nextPart = (index & 0x7F).toByte
      index2 = index2 >>> 7
      if (index2 != 0) nextPart = (nextPart | 0x80).toByte
      os.write(nextPart)

      index2 != 0
    do ()

  def writeInt16(os: OutputStream, value: Short): Unit = {
    os.write(value.toByte)
    os.write(value >>> 8)
  }
}

