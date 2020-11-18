package dev.argon.backend.jvm.classmodule;

public class MethodIdentifier {
    public MethodIdentifier(String className, String methodName, String descriptor) {
        this.className = className;
        this.methodName = methodName;
        this.descriptor = descriptor;
    }


    public final String className;
    public final String methodName;
    public final String descriptor;

    @Override
    public int hashCode() {
        return className.hashCode() * 13 + methodName.hashCode() * 7 + descriptor.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof MethodIdentifier)) return false;
        var other = (MethodIdentifier)obj;

        return className.equals(other.className) && methodName.equals(other.methodName) && descriptor.equals(other.descriptor);
    }
}
