package dev.argon.vm;

public interface Tube {
    Name tubeName();
    int tubeNum();

    ConstantPoolEntry getExport(Name name) throws Exception;

    ConstantPoolEntry getEntry(int id);
}
