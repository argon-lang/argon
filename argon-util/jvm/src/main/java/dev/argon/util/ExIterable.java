package dev.argon.util;

import org.jetbrains.annotations.Nullable;

public interface ExIterable<T> {
    ExIterator<T> iterator() throws Exception;

    default <U> ExIterable<U> map(ExFunction<T, U> f) {
        return new ExIterable<U>() {
            @Override
            public ExIterator<U> iterator() throws Exception {
                var iter = ExIterable.this.iterator();
                return new ExIterator<U>() {
                    @Override
                    public @Nullable U next() throws Exception {
                        T value = iter.next();

                        if(value == null) return null;
                        else return f.apply(value);
                    }

                    @Override
                    public void close() throws Exception {
                        iter.close();
                    }
                };
            }
        };
    }

    default <U> ExIterable<U> flatMap(ExFunction<T, ExIterable<U>> f) {
        return new ExIterable<U>() {
            @Override
            public ExIterator<U> iterator() throws Exception {
                var outer = ExIterable.this.iterator();
                return new ExIterator<U>() {
                    private ExIterator<U> inner = null;

                    @Override
                    public @Nullable U next() throws Exception {
                        while(true) {
                            if(inner == null) {
                                T tValue = outer.next();
                                if(tValue == null) return null;

                                inner = f.apply(tValue).iterator();
                            }

                            U uValue = inner.next();
                            if(uValue == null) {
                                var innerCopy = inner;
                                inner = null;
                                innerCopy.close();
                                continue;
                            }

                            return uValue;
                        }
                    }

                    @Override
                    public void close() throws Exception {
                        if(inner != null) inner.close();
                        outer.close();
                    }
                };
            }
        };
    }


    static <U> ExIterable<U> empty() {
        return new ExIterable<>() {
            @Override
            public ExIterator<U> iterator() throws Exception {
                return new ExIterator<U>() {
                    @Override
                    public U next() throws Exception {
                        return null;
                    }

                    @Override
                    public void close() throws Exception {

                    }
                };
            }
        };
    }

    static <U> ExIterable<U> fromIterable(Iterable<U> iterable) {
        return new ExIterable<U>() {
            @Override
            public ExIterator<U> iterator() throws Exception {
                var iter = iterable.iterator();
                return new ExIterator<U>() {
                    @Override
                    public @Nullable U next() throws Exception {
                        if(iter.hasNext()) return iter.next();
                        else return null;
                    }

                    @Override
                    public void close() throws Exception {
                    }
                };
            }
        };
    }
}

