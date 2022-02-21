package dev.argon.vm.interpreter

sealed trait VMCell

object VMCell {
  final case class ByteCell(var value: Byte) extends VMCell {
    def readVolatile: Byte = value
    def writeVolatile(x: Byte): Unit = value = x
  }

  final case class ShortCell(var value: Short) extends VMCell {
    def readVolatile: Short = value
    def writeVolatile(x: Short): Unit = value = x
  }
  
  final case class IntCell(var value: Int) extends VMCell {
    def readVolatile: Int = value
    def writeVolatile(x: Int): Unit = value = x
  }
  
  final case class LongCell(var value: Long) extends VMCell {
    def readVolatile: Long = value
    def writeVolatile(x: Long): Unit = value = x
  }
  
  final case class FloatCell(var value: Float) extends VMCell {
    def readVolatile: Float = value
    def writeVolatile(x: Float): Unit = value = x
  }
  
  final case class DoubleCell(var value: Double) extends VMCell {
    def readVolatile: Double = value
    def writeVolatile(x: Double): Unit = value = x
  }
  
  final case class RefCell(var value: AnyRef | Null) extends VMCell {
    def readVolatile: AnyRef | Null = value
    def writeVolatile(x: AnyRef | Null): Unit = value = x
  }
  
}

