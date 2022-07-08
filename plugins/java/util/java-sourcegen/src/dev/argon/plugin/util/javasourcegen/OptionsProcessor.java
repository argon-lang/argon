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
        Map<ExecutableElement, String> optionNames = new HashMap<>();
        Map<ExecutableElement, String> optionDescriptions = new HashMap<>();

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

            if(optionNameAnnValue == null || !(optionNameAnnValue.getValue() instanceof String optionName)) {
                messager.printMessage(Diagnostic.Kind.ERROR, "Invalid option name for " + ee);
                return;
            }

            if(optionDescAnnValue == null || !(optionDescAnnValue.getValue() instanceof String optionDesc)) {
                messager.printMessage(Diagnostic.Kind.ERROR, "Invalid option description for " + ee);
                return;
            }

            optionNames.put(ee, optionName);
            optionDescriptions.put(ee, optionDesc);
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

        if(!isOutput) {
            String builderClassName = optClassName + "Builder";
            var builderFile = processingEnv.getFiler().createSourceFile(builderClassName);

            try(PrintWriter builder2 = new PrintWriter(builderFile.openWriter())) {
                var builder = builder2;

                if(optClassPackage != null) {
                    builder.print("package ");
                    builder.print(optClassPackage);
                    builder.println(";");
                }

                builder.println("import org.checkerframework.checker.nullness.qual.NonNull;");
                builder.println("import org.checkerframework.checker.nullness.qual.Nullable;");

                builder.print("class ");
                builder.print(optClassShortName);
                builder.println("Builder<E extends Exception> {");
                builder.print("\tpublic ");
                builder.print(optClassShortName);
                builder.println("Builder() {");

                for(var method : methods) {
                    if(isListType(method.getReturnType())) {
                        builder.print("\t\tthis.");
                        builder.print(method.getSimpleName().toString());
                        builder.println("_value = new java.util.ArrayList<>();");
                    }
                }

                builder.println("\t}");

                for(var method : methods) {
                    builder.print("\tprivate ");
                    printTypeNoAnn(builder, method.getReturnType());
                    builder.print(" ");
                    builder.print(method.getSimpleName().toString());
                    builder.println("_value;");
                }

                printOptionInfoClasses(builder, isOutput, methods, optClassName, optionNames, optionDescriptions);

                builder.print("\tpublic ");
                builder.print(t.getSimpleName());
                builder.println("<E> build() throws dev.argon.plugin.api.options.MissingOptionException {");
                for(var method : methods) {
                    builder.print("\t\tvar ");
                    builder.print(method.getSimpleName());
                    builder.print("_value = this.");
                    builder.print(method.getSimpleName());
                    builder.println("_value;");

                    if(isNonNull(method.getReturnType()) && !isListType(method.getReturnType())) {
                        builder.print("\t\tif(");
                        builder.print(method.getSimpleName());
                        builder.println("_value == null) {");
                        builder.print("\t\t\tthrow new dev.argon.plugin.api.options.MissingOptionException(\"");
                        builder.print(StringEscapeUtils.escapeJava(optionNames.get(method)));
                        builder.println("\");");
                        builder.println("\t\t}");
                    }
                }
                builder.print("\t\treturn new ");
                builder.print(t.getSimpleName());
                builder.print("<E>(");
                for(int i = 0; i < methods.size(); ++i) {
                    if(i > 0) {
                        builder.print(", ");
                    }

                    builder.print("");
                    builder.print(methods.get(i).getSimpleName());
                    builder.print("_value");
                }
                builder.println(");");
                builder.println("\t}");



                builder.println("}");

                builder.flush();
            }
        }

        String handlerClassName = optClassName + "Handler";
        var handlerFile = processingEnv.getFiler().createSourceFile(handlerClassName);

        try(PrintWriter handler2 = new PrintWriter(handlerFile.openWriter())) {
            var handler = handler2;

            if(optClassPackage != null) {
                handler.print("package ");
                handler.print(optClassPackage);
                handler.println(";");
            }
            
            handler.print("class ");
            handler.print(optClassShortName);
            if(isOutput) {
                handler.print("Handler<E extends Exception> implements dev.argon.plugin.api.options.OutputHandler<E, ");
                handler.print(optClassName);
                handler.println("<E>> {");

                handler.println("\t@Override");
                handler.print("\tpublic java.util.Set<dev.argon.plugin.api.options.OutputInfo<? extends dev.argon.plugin.api.resource.Resource<E>, ");
                handler.print(optClassName);
                handler.println("<E>>> options() {");

                handler.print("\t\tjava.util.Set<dev.argon.plugin.api.options.OutputInfo<? extends dev.argon.plugin.api.resource.Resource<E>, ");
                handler.print(optClassName);
                handler.println("<E>>> options = new java.util.HashSet<>();");

                for(var method : methods) {
                    var methodName = method.getSimpleName().toString();
                    handler.print("\t\toptions.add(new ");
                    handler.print(methodName.substring(0, 1).toUpperCase(Locale.US));
                    handler.print(methodName.substring(1));
                    handler.println("Info<E>());");
                }

                handler.println("\t\treturn options;");
                handler.println("\t}");

                printOptionInfoClasses(handler, isOutput, methods, optClassName, optionNames, optionDescriptions);

            }
            else {
                handler.print("Handler<E extends Exception> implements dev.argon.plugin.api.options.OptionHandler<E, ");
                handler.print(optClassName);
                handler.print("<E>, ");
                handler.print(optClassName);
                handler.println("Builder<E>> {");


                handler.println("\t@Override");
                handler.print("\tpublic java.util.Set<dev.argon.plugin.api.options.OptionInfo<E, ?, ");
                handler.print(optClassName);
                handler.print("<E>, ");
                handler.print(optClassName);
                handler.println("Builder<E>>> options() {");

                handler.print("\t\tjava.util.Set<dev.argon.plugin.api.options.OptionInfo<E, ?, ");
                handler.print(optClassName);
                handler.print("<E>, ");
                handler.print(optClassName);
                handler.println("Builder<E>>> options = new java.util.HashSet<>();");

                for(var method : methods) {
                    var methodName = method.getSimpleName().toString();
                    handler.print("\t\toptions.add(new ");
                    handler.print(optClassName);
                    handler.print("Builder.");
                    handler.print(methodName.substring(0, 1).toUpperCase(Locale.US));
                    handler.print(methodName.substring(1));
                    handler.println("Info());");
                }

                handler.println("\t\treturn options;");
                handler.println("\t}");

                handler.println("\t@Override");
                handler.print("\tpublic ");
                handler.print(optClassName);
                handler.println("Builder<E> createBuilder() {");
                handler.print("\t\treturn new ");
                handler.print(optClassName);
                handler.println("Builder<E>();");
                handler.println("\t}");

                handler.println("\t@Override");
                handler.print("\tpublic ");
                handler.print(optClassName);
                handler.print("<E> build(");
                handler.print(optClassName);
                handler.println("Builder<E> builder) throws dev.argon.plugin.api.options.MissingOptionException {");
                handler.println("\t\treturn builder.build();");
                handler.println("\t}");


            }

            

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

    private boolean isNonNull(TypeMirror tm) {
        for(var ann : tm.getAnnotationMirrors()) {
            if(((TypeElement)ann.getAnnotationType().asElement()).getQualifiedName().toString().equals("org.checkerframework.checker.nullness.qual.NonNull")) {
                return true;
            }
        }

        return false;
    }

    private boolean isListType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("java.util.List");
    }

    private boolean isStringType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("java.lang.String");
    }

    private boolean isPathType(TypeMirror tm) {
        var typeUtils = processingEnv.getTypeUtils();
        return ((TypeElement)typeUtils.asElement(tm)).getQualifiedName().toString().equals("java.nio.file.Path");
    }

    private void printScalarTypeExpr(PrintWriter writer, TypeMirror tm) {
        if(isStringType(tm)) {
            writer.print("value");
        }
        else if(isPathType(tm)) {
            writer.print("java.nio.file.Path.of(value)");
        }
        else {
            var messager = processingEnv.getMessager();
            messager.printMessage(Diagnostic.Kind.ERROR, "Unsupported option type: " + tm);
        }
    }

    private void printOptionInfoClasses(PrintWriter writer, boolean isOutput, Iterable<ExecutableElement> methods, String optClassName, Map<ExecutableElement, String> optionNames, Map<ExecutableElement, String> optionDescriptions) {
        for(var method : methods) {
            var methodName = method.getSimpleName().toString();
            writer.print("\tpublic static class ");
            writer.print(methodName.substring(0, 1).toUpperCase(Locale.US));
            writer.print(methodName.substring(1));
            writer.print("Info<E extends Exception> implements dev.argon.plugin.api.options.");
            if(isOutput) {
                writer.print("OutputInfo<");
            }
            else {
                writer.print("OptionInfo<E, ");
            }
            printTypeNoAnn(writer, method.getReturnType());
            writer.print(", ");
            writer.print(optClassName);
            writer.print("<E>");
            if(!isOutput) {
                writer.print(", ");
                writer.print(optClassName);
                writer.print("Builder<E>");
            }
            writer.println("> {");

            writer.println("\t\t@Override");
            writer.println("\t\tpublic String name() {");
            writer.print("\t\t\treturn \"");
            writer.print(StringEscapeUtils.escapeJava(optionNames.get(method)));
            writer.println("\";");
            writer.println("\t\t}");

            writer.println("\t\t@Override");
            writer.println("\t\tpublic String description() {");
            writer.print("\t\t\treturn \"");
            writer.print(StringEscapeUtils.escapeJava(optionDescriptions.get(method)));
            writer.println("\";");
            writer.println("\t\t}");

            if(!isOutput) {
                writer.println("\t\t@Override");
                writer.print("\t\tpublic ");
                writer.print(optClassName);
                writer.print("Builder<E> addOptionValue(");
                writer.print(optClassName);
                writer.println("Builder<E> prev, String value) throws java.io.IOException, dev.argon.plugin.api.options.InvalidOptionValueException, dev.argon.plugin.api.options.DuplicateOptionValueException {{");
                writer.print("\t\t\tvar next = new ");
                writer.print(optClassName);
                writer.println("Builder<E>();");
                for(var method2 : methods) {
                    if(method.getSimpleName().equals(method2.getSimpleName())) {
                        if(isListType(method.getReturnType())) {
                            var elementType = ((DeclaredType)method.getReturnType()).getTypeArguments().get(0);
    
                            writer.print("\t\t\tnext.");
                            writer.print(method2.getSimpleName());
                            writer.print("_value.addAll(prev.");
                            writer.print(method2.getSimpleName());
                            writer.println("_value);");
    
                            writer.print("\t\t\tnext.");
                            writer.print(method2.getSimpleName());
                            writer.print("_value.add(");
                            printScalarTypeExpr(writer, elementType);
                            writer.println(");");
                        }
                        else {
                            writer.print("\t\t\tnext.");
                            writer.print(method2.getSimpleName());
                            writer.print("_value = ");
                            printScalarTypeExpr(writer, method.getReturnType());
                            writer.println(";");
                        }
                    }
                    else {
                        writer.print("\t\t\tnext.");
                        writer.print(method2.getSimpleName());
                        writer.print("_value = prev.");
                        writer.print(method2.getSimpleName());
                        writer.println("_value;");
                    }
                }
                writer.println("\t\t\treturn next;");
                writer.println("\t\t}}");
            }


            writer.println("\t\t@Override");
            writer.print("\t\tpublic ");
            printTypeNoAnn(writer, method.getReturnType());
            writer.print(" getValue(");
            writer.print(optClassName);
            writer.println("<E> options) {");
            writer.print("\t\t\treturn options.");
            writer.print(methodName);
            writer.println("();");
            writer.println("\t\t}");


            writer.println("\t}");

        }

    }
}
