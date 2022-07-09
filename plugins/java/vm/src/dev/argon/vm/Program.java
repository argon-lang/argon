package dev.argon.vm;

import java.util.List;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import dev.argon.plugin.api.util.InputStreamFormatReader;

public class Program {
    public static void main(String[] args) throws Exception {

        List<TubeLoader> tubes = new ArrayList<>();
        tubes.add((runtime, tubeNum) -> new dev.argon.vm.libs.core.CoreLibTube(tubeNum));

        int i = 0;
        for(; i < args.length; ++i) {
            switch(args[i]) {
                case "-t" -> {
                    ++i;
                    if(i >= args.length) {
                        System.err.println("Missing value for argument -t");
                        System.exit(1);
                        return;
                    }

                    tubes.add(loadTube(args[i]));
                }

                default -> {
                    if(args[i].startsWith("-")) {
                        System.err.println("Unknown argument: " + args[i]);
                        System.exit(1);
                        return;
                    }
                    break;
                }
            }
        }

        if(i >= args.length) {
            System.err.println("Missing value for application tube name");
            System.exit(1);
            return;
        }
        Name appTubeName = Name.parse(args[i]);
        ++i;

        if(i >= args.length) {
            System.err.println("Missing value for main function export name");
            System.exit(1);
            return;
        }
        Name mainExportName = Name.parse(args[i]);
        ++i;

        var runtime = new Runtime(tubes);

        var tube = runtime.getTube(appTubeName);
        if(tube == null) {
            System.err.println("Unknown tube name: " + appTubeName);
            System.exit(1);
            return;
        }

        var main = (FunctionDefinition)tube.getExport(mainExportName);
        
        var mainClass = runtime.classLoader().loadClass(main.className().replace("/", "."));
        var method = Arrays.stream(mainClass.getMethods()).filter(m -> m.getName().equals(main.methodName())).findFirst().get();

        method.invoke(null);
    }


    private static TubeLoader loadTube(String filename) {
        return (runtime, tubeNum) -> {
            try(var is = Files.newInputStream(Path.of(filename))) {
                var reader = new InputStreamFormatReader(is);
                var tubeFormat = dev.argon.vm.format.Tube.V1.codec.read(reader);
    
                return new TubeImpl(runtime, tubeNum, tubeFormat);
            }    
        };
    }

}
