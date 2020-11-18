package dev.argon.backend.jvm.classmodule.loader;

import java.util.ArrayList;
import java.util.List;

class IdList<T> {
    private final Object lock = new Object();
    private final List<T> list = new ArrayList<>();

    public int getId(T value) {
        synchronized (lock) {
            int index = list.indexOf(value);
            if(index >= 0) return index;

            index = list.size();
            list.add(value);
            return index;
        }
    }

    public T lookupId(int id) throws InvalidModuleException {
        synchronized (lock) {
            if(id < 0 || id >= list.size()) {
                throw new InvalidModuleException();
            }
            return list.get(id);
        }
    }

    public static class FromOne<T> extends IdList<T> {
        @Override
        public int getId(T value) {
            return super.getId(value) + 1;
        }

        @Override
        public T lookupId(int id) throws InvalidModuleException {
            return super.lookupId(id - 1);
        }
    }

    public static class Negative<T> extends IdList<T> {
        @Override
        public int getId(T value) {
            return -super.getId(value) - 1;
        }

        @Override
        public T lookupId(int id) throws InvalidModuleException {
            return super.lookupId(-(id + 1));
        }
    }
}
