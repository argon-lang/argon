package dev.argon.backend.api;

import dev.argon.nobleidl.runtime.graaljsInterop.ExceptionUtil;
import dev.argon.nobleidl.runtime.graaljsInterop.JSAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSExecutor;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class TestExecutionFailure {
	private TestExecutionFailure() {}
	
	public static JSAdapter<Throwable> jsAdapter() {
		return new JSAdapter<>() {
			@Override
			public Throwable fromJS(Context context, JSExecutor jsExecutor, Value value) {
				return ExceptionUtil.valueToThrowable(context, value);
			}

			@Override
			public Value toJS(Context context, JSExecutor jsExecutor, Throwable ex) {
				return ExceptionUtil.throwableToValue(context, ex);
			}
		};
	}
}
