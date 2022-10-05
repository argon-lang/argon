package dev.argon.argonvm.engine;

public sealed interface FrameExit extends TrampolinedFrameExit {
	public static record ParentFrame(StackFrame frame) implements FrameExit {}
	public static record Result(Object value) implements FrameExit {}
}
