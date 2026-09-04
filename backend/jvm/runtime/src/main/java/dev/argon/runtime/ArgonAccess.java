package dev.argon.runtime;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.CLASS)
@Target({ ElementType.TYPE, ElementType.METHOD, ElementType.FIELD })
public @interface ArgonAccess {
	ArgonAccessModifier value();
}
