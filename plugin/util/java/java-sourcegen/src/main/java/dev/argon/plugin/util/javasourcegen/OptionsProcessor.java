package dev.argon.plugin.util.javasourcegen;

import javax.annotation.processing.*;
import javax.lang.model.*;
import javax.lang.model.element.*;
import javax.lang.model.type.*;
import javax.tools.Diagnostic;

import java.io.IOException;
import java.io.PrintWriter;

import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Map;
import java.util.HashMap;

import org.apache.commons.text.StringEscapeUtils;

@SupportedAnnotationTypes({"dev.argon.plugin.api.options.Options", "dev.argon.plugin.api.options.Option"})
@SupportedSourceVersion(SourceVersion.RELEASE_17)
public class OptionsProcessor extends AbstractProcessor {

    private ProcessingEnvironment processingEnv;

    @Override
    public void init(ProcessingEnvironment processingEnv) {
        this.processingEnv = processingEnv;
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        var messager = processingEnv.getMessager();
    
        for(TypeElement annotation : annotations) {
            if(!annotation.getQualifiedName().toString().equals("dev.argon.plugin.api.options.Options")) {
                continue;
            }

            Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
            
            for(Element elem : annotatedElements) {
                var recordElem = (TypeElement)elem;
                if(recordElem.getKind() != ElementKind.RECORD) {
                    messager.printMessage(Diagnostic.Kind.ERROR, "Option type must be a record: " + elem);
                    continue;
                }

                AnnotationValue optionsDecodingAnnValue = null;
                for(var ann : recordElem.getAnnotationMirrors()) {
                    if(((TypeElement)ann.getAnnotationType().asElement()).getQualifiedName().equals(annotation.getQualifiedName())) {
                        for(var annElement : ann.getElementValues().entrySet()) {
                            if(annElement.getKey().getSimpleName().contentEquals("decoding")) {
                                optionsDecodingAnnValue = annElement.getValue();
                                break;
                            }
                        }
                        break;
                    }
                }

                if(optionsDecodingAnnValue == null || !(optionsDecodingAnnValue.getValue() instanceof VariableElement optionsDecoding)) {
                    messager.printMessage(Diagnostic.Kind.ERROR, "Invalid options decoding for " + elem);
                    continue;
                }

                if(recordElem.getKind() != ElementKind.RECORD) {
                    messager.printMessage(Diagnostic.Kind.ERROR, "Option type must be a record: " + elem);
                    continue;
                }

                boolean isOutput = optionsDecoding.getSimpleName().toString().equals("OUTPUT");
                

                try {
                    generateFiles(recordElem, isOutput);
                }
                catch(IOException ex) {
                    throw new RuntimeException(ex);
                }
            }
        }
    
        return true;
    }

    private void generateFiles(TypeElement t, boolean isOutput) throws IOException {
        var typeUtils = processingEnv.getTypeUtils();
        var messager = processingEnv.getMessager();

        Set<Name> recordComponents = new HashSet<>();


        for(var e : t.getEnclosedElements()) {
            if(e instanceof RecordComponentElement rce) {
                recordComponents.add(rce.getSimpleName());
            }
            
        }

        ExecutableElement constructor = null;
        List<ExecutableElement> methods = new ArrayList<>();

        for(var e : t.getEnclosedElements()) {
            if(!(e instanceof ExecutableElement ee)) {
                continue;
            }

            if(e.getKind() == ElementKind.CONSTRUCTOR) {
                if(constructor != null) {
                    messager.printMessage(Diagnostic.Kind.ERROR, "Option type may only have a single constructor: " + t);
                    return;
                }

                constructor = ee;
                continue;
            }

            if(!recordComponents.contains(ee.getSimpleName())) {
                continue;
            }
            
            methods.add(ee);


            AnnotationValue optionNameAnnValue = null;
            AnnotationValue optionDescAnnValue = null;
            for(var ann : ee.getAnnotationMirrors()) {
                if(((TypeElement)ann.getAnnotationType().asElement()).getQualifiedName().toString().equals("dev.argon.plugin.api.options.Option")) {
                    for(var annElement : ann.getElementValues().entrySet()) {
                        if(annElement.getKey().getSimpleName().contentEquals("name")) {
                            optionNameAnnValue = annElement.getValue();
                        }
                        else if(annElement.getKey().getSimpleName().contentEquals("description")) {
                            optionDescAnnValue = annElement.getValue();
                        }
                    }
                    break;
                }
            }
        }

        String optClassName = t.getQualifiedName().toString();

        String optClassPackage = null;
        {
            int optClassDot = optClassName.lastIndexOf('.');
            if(optClassDot >= 0) {
                optClassPackage = optClassName.substring(0, optClassDot);
            }
        }

        String optClassShortName = t.getSimpleName().toString();


        String handlerClassName = optClassName + (isOutput ? "Handler" : "Codec");
        var handlerFile = processingEnv.getFiler().createSourceFile(handlerClassName);

        try(PrintWriter handler2 = new PrintWriter(handlerFile.openWriter())) {
            var handler = handler2;

            if(optClassPackage != null) {
                handler.print("package ");
                handler.print(optClassPackage);
                handler.println(";");
            }

            handler.println("import org.jetbrains.annotations.NotNull;");
            
            handler.print("class ");
            handler.print(optClassShortName);
            if(isOutput) {
                handler.print("Handler<E extends Throwable> implements dev.argon.plugin.api.options.OutputHandler<E, ");
                handler.print(optClassName);
                handler.println("<E>> {");

                handler.println("\t@Override");
                handler.print("\tpublic @NotNull java.util.Map<java.util.List<String>, dev.argon.plugin.api.options.OutputInfo<E, ");
                handler.print(optClassName);
                handler.println("<E>>> options() {");

                handler.print("\t\tjava.util.Map<java.util.List<String>, dev.argon.plugin.api.options.OutputInfo<E, ");
                handler.print(optClassName);
                handler.println("<E>>> options = new java.util.HashMap<>();");

                for(var method : methods) {
                    var methodName = method.getSimpleName().toString();

                    handler.print("\t\toptions.put(java.util.List.of(\"");
                    handler.print(StringEscapeUtils.escapeJava(methodName));
                    handler.print("\"), new ");
                    handler.print(methodName.substring(0, 1).toUpperCase(Locale.US));
                    handler.print(methodName.substring(1));
                    handler.println("Info<E>());");
                }

                handler.println("\t\treturn options;");
                handler.println("\t}");

                printOptionInfoClasses(handler, methods, optClassName);

            }
            else {
                handler.print("Codec<E extends Throwable> implements dev.argon.plugin.api.options.OptionCodec<E, ");
                handler.print(optClassName);
                handler.println("<E>> {");

                handler.println("\t@Override");
                handler.print("\tpublic @NotNull dev.argon.plugin.api.tube.Toml encode(@NotNull dev.argon.plugin.api.ResourceRecorder<E> recorder, @NotNull ");
                handler.print(optClassName);
                handler.println("<E> value) throws E, java.io.IOException, InterruptedException {");
                handler.println("\t\treturn dev.argon.plugin.api.tube.Toml.newBuilder()");
                handler.println("\t\t\t.setTable(");
                handler.println("\t\t\t\tdev.argon.plugin.api.tube.TomlTable.newBuilder()");
                for(var method : methods) {
                    var methodName = method.getSimpleName().toString();
                    handler.print("\t\t\t\t\t.addElements(dev.argon.plugin.api.tube.TomlKeyValue.newBuilder().setKey(\"");
                    handler.print(StringEscapeUtils.escapeJava(methodName));
                    handler.print("\").setValue(");
                    printOptionWrite(handler, methodName, method.getReturnType());
                    handler.println(").build())");
                }
                handler.println("\t\t\t\t\t.build()");
                handler.println("\t\t\t)");
                handler.println("\t\t\t.build();");
                handler.println("\t}");

                handler.println("\t@Override");
                handler.print("\tpublic @NotNull ");
                handler.print(optClassName);
                handler.println("<E> decode(@NotNull dev.argon.plugin.api.ResourceFactory<E> factory, @NotNull dev.argon.plugin.api.tube.Toml value) throws dev.argon.plugin.api.options.OptionDecodeException {");
                handler.println("\t\tdev.argon.plugin.api.tube.TomlTable table;");
                handler.println("\t\tif(value.hasTable()) {");
                handler.println("\t\t\ttable = value.getTable();");
                handler.println("\t\t}");
                handler.println("\t\telse {");
                handler.println("\t\t\tthrow new dev.argon.plugin.api.options.OptionDecodeException(\"Expected table\");");
                handler.println("\t\t}");
                handler.print("\t\treturn new ");
                handler.print(optClassName);
                handler.println("<E>(");
                for(int i = 0; i < methods.size(); ++i) {
                    var method = methods.get(i);
                    var methodName = method.getSimpleName().toString();
                    handler.print("\t\t\t");
                    printOptionRead(handler, methodName, method.getReturnType());
                    if(i < methods.size() - 1) {
                        handler.print(",");
                    }
                    handler.println();
                }
                handler.println("\t\t);");
                handler.println("\t}");


            }

            handler.println("\tprivate String getStringProp(@NotNull dev.argon.plugin.api.tube.TomlTable table, @NotNull String name) throws dev.argon.plugin.api.options.OptionDecodeException {");
            handler.println("\t\tfor(var element : table.getElementsList()) {");
            handler.println("\t\t\tif(element.getKey().equals(name)) {");
            handler.println("\t\t\t\tdev.argon.plugin.api.tube.Toml value = element.getValue();");
            handler.println("\t\t\t\tif(value.hasStringValue()) {");
            handler.println("\t\t\t\t\treturn value.getStringValue();");
            handler.println("\t\t\t\t}");
            handler.println("\t\t\t\telse {");
            handler.println("\t\t\t\t\tthrow new dev.argon.plugin.api.options.OptionDecodeException(\"Expected string\");");
            handler.println("\t\t\t\t}");
            handler.println("\t\t\t}");
            handler.println("\t\t}");
            handler.println("\t\tthrow new dev.argon.plugin.api.options.OptionDecodeException(\"Missing key \" + name);");
            handler.println("\t}");

            

            handler.println("}");

            handler.flush();
        }

    }

    private void printTypeNoAnn(PrintWriter writer, TypeMirror tm) {
        var dt = (DeclaredType)tm;

        writer.print(((TypeElement)dt.asElement()).getQualifiedName());
        var params = dt.getTypeArguments();
        if(params.size() > 0) {
            writer.print("<");
            for(int i = 0; i < params.size(); ++i) {
                if(i > 0) {
                    writer.print(", ");
                }
                writer.print(params.get(i));
            }
            writer.print(">");
        }
    }

    private boolean isListType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("java.util.List");
    }

    private boolean isStringType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("java.lang.String");
    }

    private boolean isBinaryResourceType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("dev.argon.plugin.api.BinaryResource");
    }

    private void printOptionInfoClasses(PrintWriter writer, Iterable<ExecutableElement> methods, String optClassName) {
        for(var method : methods) {
            var methodName = method.getSimpleName().toString();
            writer.print("\tpublic static class ");
            writer.print(methodName.substring(0, 1).toUpperCase(Locale.US));
            writer.print(methodName.substring(1));
            writer.print("Info<E extends Throwable> implements dev.argon.plugin.api.options.OutputInfo<E, ");
            writer.print(optClassName);
            writer.println("<E>> {");



            writer.println("\t\t@Override");
            writer.print("\t\tpublic @NotNull ");
            printTypeNoAnn(writer, method.getReturnType());
            writer.print(" getValue(@NotNull ");
            writer.print(optClassName);
            writer.println("<E> options) {");
            writer.print("\t\t\treturn options.");
            writer.print(methodName);
            writer.println("();");
            writer.println("\t\t}");


            writer.println("\t}");

        }

    }

    private void printOptionWrite(PrintWriter writer, String optionName, TypeMirror optType) {
        if(isStringType(optType)) {
            writer.print("dev.argon.plugin.api.tube.Toml.newBuilder().setStringValue(value.");
            writer.print(optionName);
            writer.print("())");
        }
        else if(isBinaryResourceType(optType)) {
            writer.print("dev.argon.plugin.api.tube.Toml.newBuilder().setStringValue(recorder.recordBinaryResource(value.");
            writer.print(optionName);
            writer.print("()))");
        }
        else {
            throw new RuntimeException("Unexpected option type: " + optType);
        }
    }

    private void printOptionRead(PrintWriter writer, String optionName, TypeMirror optType) {
        if(isStringType(optType)) {
            writer.print("getStringProp(table, \"");
            writer.print(StringEscapeUtils.escapeJava(optionName));
            writer.print("\")");
        }
        if(isBinaryResourceType(optType)) {
            writer.print("factory.binaryResource(getStringProp(table, \"");
            writer.print(StringEscapeUtils.escapeJava(optionName));
            writer.print("\"))");
        }
        else {
            throw new RuntimeException("Unexpected option type: " + optType);
        }
    }

}
