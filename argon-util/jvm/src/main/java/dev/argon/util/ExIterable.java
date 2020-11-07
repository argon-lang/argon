package dev.argon.util;

public interface ExIterable<T> {
    ExIterator<T> iterator() throws Exception;

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
}

