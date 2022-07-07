package dev.argon.vm;

public interface Tube {
    int tubeNum();

    ConstantPoolEntry getExport(Name name) throws Exception;
}
