package dev.argon.vm.interpreter;

import java.lang.invoke.*;

public class VarHandleHelper {
    private VarHandleHelper() {}

    public static byte readVolatileByte(VMCell.ByteCell cell, VarHandle handle) {
        return (byte)handle.getVolatile(cell);
    }

    public static short readVolatileShort(VMCell.ShortCell cell, VarHandle handle) {
        return (short)handle.getVolatile(cell);
    }

    public static int readVolatileInt(VMCell.IntCell cell, VarHandle handle) {
        return (int)handle.getVolatile(cell);
    }

    public static long readVolatileLong(VMCell.LongCell cell, VarHandle handle) {
        return (long)handle.getVolatile(cell);
    }

    public static float readVolatileFloat(VMCell.FloatCell cell, VarHandle handle) {
        return (float)handle.getVolatile(cell);
    }

    public static double readVolatileDouble(VMCell.DoubleCell cell, VarHandle handle) {
        return (double)handle.getVolatile(cell);
    }

    public static Object readVolatileRef(VMCell.RefCell cell, VarHandle handle) {
        return handle.getVolatile(cell);
    }

    public static void writeVolatileByte(VMCell.ByteCell cell, VarHandle handle, byte value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileShort(VMCell.ShortCell cell, VarHandle handle, short value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileInt(VMCell.IntCell cell, VarHandle handle, int value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileLong(VMCell.LongCell cell, VarHandle handle, long value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileFloat(VMCell.FloatCell cell, VarHandle handle, float value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileDouble(VMCell.DoubleCell cell, VarHandle handle, double value) {
        handle.setVolatile(cell, value);
    }

    public static void writeVolatileRef(VMCell.RefCell cell, VarHandle handle, Object value) {
        handle.setVolatile(cell, value);
    }
}
