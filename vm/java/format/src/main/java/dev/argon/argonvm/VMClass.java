package dev.argon.argonvm;

import java.util.List;
import java.util.Map;

public interface VMClass {
	VMClass baseClass();
	List<VMType> fields();
	List<SlotSignature> slots();
	long getImplementationFunctionId(SlotKey slotKey);
}
