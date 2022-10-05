package dev.argon.argonvm.engine;

import dev.argon.argonvm.Program;
import dev.argon.argonvm.SlotKey;
import dev.argon.argonvm.VMClass;

import java.util.HashMap;
import java.util.Map;

public class VMObject {
	public VMObject(Program program, VMClass objectClass) {
		this.objectClass = objectClass;

		for(VMClass baseClass = objectClass; baseClass != null; baseClass = baseClass.baseClass()) {
			long classId = program.getClassIndex(baseClass);
			for(int i = 0; i < baseClass.fields().size(); ++i) {
				var fieldId = new FieldId(classId, i);
				fields.put(fieldId, baseClass.fields().get(i).defaultValue());
			}
		}
	}

	private final VMClass objectClass;

	private final Map<FieldId, Object> fields = new HashMap<>();


	public VMClass vmClass() {
		return objectClass;
	}

	public Object loadField(FieldId field) {
		return fields.get(field);
	}

	public void storeField(FieldId field, Object value) {
		fields.put(field, value);
	}


}
