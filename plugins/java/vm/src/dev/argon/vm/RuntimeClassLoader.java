package dev.argon.vm;

import java.util.regex.Pattern;
import org.objectweb.asm.ClassWriter;

final class RuntimeClassLoader extends ClassLoader {
    RuntimeClassLoader(ClassLoader parent, Runtime runtime) {
        super(parent);
        this.runtime = runtime;
    }

    static final String PACKAGE_PREFIX = "dev.argon.vm.app.";
    private final Runtime runtime;
    private final Pattern classNamePattern = Pattern.compile("tube(\\d+).([:alpha:]+)(\\d+)");

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        if(name.startsWith(PACKAGE_PREFIX)) {
            var matcher = classNamePattern.matcher(name.substring(PACKAGE_PREFIX.length()));
            if(!matcher.find()) {
                throw new ClassNotFoundException("Invalid format for Argon VM class: " + name);
            }

            int tubeNum = Integer.parseInt(matcher.group(0));
            String constantPoolEntryType = matcher.group(1);
            int groupIndex = Integer.parseInt(matcher.group(2));

            var tube = runtime.getTube(tubeNum);

            var writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
            switch(constantPoolEntryType) {
                case "Function" -> ((FunctionDefinition)tube.getEntry(groupIndex)).visitClass(writer);

                default -> throw new ClassNotFoundException("Invalid type of Argon VM class " + constantPoolEntryType + " for " + name);
            }

            byte[] classData = writer.toByteArray();
            return defineClass(name, classData, 0, classData.length);
        }
        else {
            return super.findClass(name);
        }
    }
}
