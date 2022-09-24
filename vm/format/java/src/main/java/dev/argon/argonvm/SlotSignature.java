package dev.argon.argonvm;

import java.util.List;

public record SlotSignature(List<VMType> parameterTypes, VMType returnType) {
}
