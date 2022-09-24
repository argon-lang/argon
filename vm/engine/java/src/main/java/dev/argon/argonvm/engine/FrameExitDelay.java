package dev.argon.argonvm.engine;

import dev.argon.argonvm.NativeTrampoline;

public record FrameExitDelay(NativeTrampoline trampoline) implements TrampolinedFrameExit {
}
