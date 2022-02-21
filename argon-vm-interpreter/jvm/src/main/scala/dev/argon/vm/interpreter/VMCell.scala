package dev.argon.vm.interpreter

import java.lang.invoke.*

sealed trait VMCell

object VMCell {
  final case class ByteCell(var value: Byte) extends VMCell {
    def readVolatile: Byte = VarHandleHelper.readVolatileByte(this, ByteCell.varHandle)
    def writeVolatile(x: Byte): Unit = VarHandleHelper.writeVolatileByte(this, ByteCell.varHandle, x)
  }
  object ByteCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[ByteCell], "value", classOf[Byte])
  }

  final case class ShortCell(var value: Short) extends VMCell {
    def readVolatile: Short = VarHandleHelper.readVolatileShort(this, ShortCell.varHandle)
    def writeVolatile(x: Short): Unit = VarHandleHelper.writeVolatileShort(this, ShortCell.varHandle, x)
  }
  object ShortCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[ShortCell], "value", classOf[Short])
  }

  final case class IntCell(var value: Int) extends VMCell {
    def readVolatile: Int = VarHandleHelper.readVolatileInt(this, IntCell.varHandle)
    def writeVolatile(x: Int): Unit = VarHandleHelper.writeVolatileInt(this, IntCell.varHandle, x)
  }
  object IntCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[IntCell], "value", classOf[Int])
  }

  final case class LongCell(var value: Long) extends VMCell {
    def readVolatile: Long = VarHandleHelper.readVolatileLong(this, LongCell.varHandle)
    def writeVolatile(x: Long): Unit = VarHandleHelper.writeVolatileLong(this, LongCell.varHandle, x)
  }
  object LongCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[LongCell], "value", classOf[Long])
  }
  
  final case class FloatCell(var value: Float) extends VMCell {
    def readVolatile: Float = VarHandleHelper.readVolatileFloat(this, FloatCell.varHandle)
    def writeVolatile(x: Float): Unit = VarHandleHelper.writeVolatileFloat(this, FloatCell.varHandle, x)
  }
  object FloatCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[FloatCell], "value", classOf[Float])
  }
  
  final case class DoubleCell(var value: Double) extends VMCell {
    def readVolatile: Double = VarHandleHelper.readVolatileDouble(this, DoubleCell.varHandle)
    def writeVolatile(x: Double): Unit = VarHandleHelper.writeVolatileDouble(this, DoubleCell.varHandle, x)
  }
  object DoubleCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[DoubleCell], "value", classOf[Double])
  }
  
  final case class RefCell(var value: AnyRef | Null) extends VMCell {
    def readVolatile: AnyRef | Null = VarHandleHelper.readVolatileRef(this, RefCell.varHandle)
    def writeVolatile(x: AnyRef | Null): Unit = VarHandleHelper.writeVolatileRef(this, RefCell.varHandle, x)
  }
  object RefCell {
    private val varHandle: VarHandle =
      MethodHandles.lookup()
        .findVarHandle(classOf[RefCell], "value", classOf[AnyRef])
  }
  
}

