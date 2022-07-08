package dev.argon.plugin.api;

import java.util.Map;
import java.util.List;
import java.io.IOException;

public interface OutputExecutor<TOutput> {
    ExecutionResult execute(Map<List<String>, TOutput> libraries, TOutput buildOutput) throws IOException;
}
