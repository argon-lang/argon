package dev.argon.vm;

import java.util.Map;
import java.util.HashMap;
import org.checkerframework.checker.nullness.qual.Nullable;

final class TubeImpl implements Tube {
    TubeImpl(Runtime runtime, Name tubeName, int tubeNum, dev.argon.vm.format.Tube.V1 tube) {
        this.runtime = runtime;
        this.tubeName = tubeName;
        this.tubeNum = tubeNum;
        constantPool = tube.constantPool();

        entries = new ConstantPoolEntry[constantPool.entries().size()];

        exports = new HashMap<>();
        for(int i = 0; i < tube.exports().size(); ++i) {
            var export = tube.exports().get(i);
            exports.put(new Name(export.key()), export.value().num().intValue());
        }
    }

    private final Runtime runtime;
    private final Name tubeName;
    final int tubeNum;
    private final dev.argon.vm.format.ConstantPool.V1 constantPool;
    private final @Nullable ConstantPoolEntry[] entries;
    private final Map<Name, Integer> exports;

    @Override
    public int tubeNum() {
        return tubeNum;
    }

    @Override
    public ConstantPoolEntry getExport(Name name) throws Exception {
        Integer exportIndex = exports.get(name);
        if(exportIndex == null) {
            throw new UnknownExportException(tubeName, name);
        }

        return getEntry(exportIndex);
    }


    synchronized ConstantPoolEntry getEntry(int index) {
        var entry = entries[index];
        if(entry == null) {
            entry = createEntry(constantPool.entries().get(index), index);
            entries[index] = entry;
        }
        return entry;
    }

    private ConstantPoolEntry createEntry(dev.argon.vm.format.ConstantPoolEntry.V1 entry, int entryIndex) {
        return switch(entry) {
            case dev.argon.vm.format.ConstantPoolEntry.V1.Function function ->
                new FunctionDefinitionImpl(runtime, this, entryIndex, function.function());

            default -> throw new RuntimeException("Not implemented");
        };
    }
}
