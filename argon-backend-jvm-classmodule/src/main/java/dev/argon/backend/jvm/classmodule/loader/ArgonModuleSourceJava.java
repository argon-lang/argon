package dev.argon.backend.jvm.classmodule.loader;

import com.google.protobuf.Empty;
import dev.argon.backend.jvm.classmodule.Constants;
import dev.argon.backend.jvm.classmodule.MethodIdentifier;
import dev.argon.backend.jvm.classmodule.jdkloader.ClassPath;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaLibrary;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaLibraryVisitor;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaModule;
import dev.argon.module.java_format.*;
import dev.argon.util.Delayed;
import dev.argon.util.ExIterable;
import dev.argon.util.ExIterator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.objectweb.asm.*;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;
import scala.NotImplementedError;

import java.io.InputStream;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class ArgonModuleSourceJava implements ArgonModuleSourceImpure {
    public ArgonModuleSourceJava(JavaArModuleLoaderFactory factory, JavaLibrary library) {
        this.factory = factory;
        this.library = library;
    }

    private final JavaArModuleLoaderFactory factory;
    private final JavaLibrary library;

    private static final String primitiveClassPrefix = "Ar/Java/";

    private FileID getFileId(String className) {
        // TODO: Use the outer class to determine ID
        return FileID.newBuilder().setId(1).build();
    }


    private final IdList<String> packageIds = new IdList<>();
    private final IdList<String> traitDefIds = new IdList.FromOne<>();
    private final IdList<String> traitRefIds = new IdList.Negative<>();
    private final IdList<String> classDefIds = new IdList.FromOne<>();
    private final IdList<String> classRefIds = new IdList.Negative<>();
    private final IdList<MethodIdentifier> methodDefIds = new IdList.FromOne<>();
    private final ConcurrentMap<Integer, Delayed<MethodDefinition>> methodDefs = new ConcurrentHashMap<>();
    private final ConcurrentMap<Integer, MethodReference> methodRefs = new ConcurrentHashMap<>();


    private Namespace namespaceFromSlashPackage(String pkg) {
        var pkgParts = pkg.split("/", -1);
        if(pkgParts.length == 1 && pkgParts[0].isEmpty()) pkgParts = new String[0];

        return Namespace.newBuilder()
            .addAllPath(Arrays.asList(pkgParts))
            .build();
    }

    @Override
    public Metadata metadata() throws Exception {
        return Metadata.newBuilder()
            .setFormatVersion(ModuleFormatVersion.currentVersion())
            .setName(library.visit(new LibraryNameVisitor()))
            .setModuleType(ModuleType.InterfaceModule)
            .build();
    }

    @Override
    public ModuleReferencesList references() throws Exception {
        var listBuilder = ModuleReferencesList.newBuilder();
        var references = library.visit(new LibraryReferencesVisitor());

        try(var iter = references.iterator()) {
            for (var ref = iter.next(); ref != null; ref = iter.next()) {
                listBuilder.addReferences(ref);
            }
        }

        return listBuilder.build();
    }

    @Override
    public ExIterable<NamespaceDeclaration> namespaces() throws Exception {
        return library
            .packages()
            .map(pkg -> NamespaceDeclaration.newBuilder()
                .setNs(namespaceFromSlashPackage(pkg))
                .setId(packageIds.getId(pkg))
                .build()
            );
    }

    @Override
    public ExIterable<GlobalDeclarationElement> namespaceElements(int id) throws Exception {
        String pkg = packageIds.lookupId(id);
        var classes = library.classesInPackage(pkg);

        return new ExIterable<GlobalDeclarationElement>() {
            @Override
            public ExIterator<GlobalDeclarationElement> iterator() throws Exception {
                var iter = classes.iterator();

                return new ExIterator<GlobalDeclarationElement>() {
                    @Override
                    public @Nullable GlobalDeclarationElement next() throws Exception {
                        InputStream stream = null;
                        try {
                            String className;
                            ClassReader classReader;
                            do {
                                className = iter.next();
                                if(className == null) return null;

                                if(stream != null) stream.close();
                                stream = null;

                                stream = library.findClass(className);
                                if(stream == null) throw new InvalidModuleException();

                                classReader = new ClassReader(stream);
                            } while((classReader.getAccess() & Opcodes.ACC_MODULE) == Opcodes.ACC_MODULE);

                            int access = classReader.getAccess();
                            boolean isTrait = (access & Opcodes.ACC_INTERFACE) == Opcodes.ACC_INTERFACE;
                            var parts = classReader.getClassName().split("/", -1);

                            AccessModifier accessModifier;
                            if((access & Opcodes.ACC_PUBLIC) == Opcodes.ACC_PUBLIC) {
                                accessModifier = AccessModifier.Public;
                            }
                            else {
                                accessModifier = AccessModifier.Internal;
                            }

                            var classSignature = readSignature(classReader);
                            int typeParamCount;
                            if(classSignature == null) {
                                typeParamCount = 0;
                            }
                            else {
                                var sigReader = new SignatureReader(classSignature);

                                var counter = new TypeParamCounterVisitor();
                                sigReader.accept(counter);
                                typeParamCount = counter.typeParamCount;
                            }

                            var erasedSigBuilder = ErasedSignatureParameterOnly.newBuilder();
                            for(int i = 0; i < typeParamCount; ++i) {
                                erasedSigBuilder.addParameterTypes(SigType.newBuilder().setEmpty(Empty.newBuilder().build()).build());
                            }

                            var declaration = GlobalDeclarationType.newBuilder()
                                    .setId(isTrait ? traitDefIds.getId(className) : classDefIds.getId(className))
                                    .setName(GlobalName.newBuilder().setNormalName(parts[parts.length - 1]).build())
                                    .setAccessModifier(accessModifier)
                                    .setSig(erasedSigBuilder.build())
                                    .build();

                            if(isTrait) {
                                return new GlobalDeclarationElement.TraitElement(declaration);
                            }
                            else {
                                return new GlobalDeclarationElement.ClassElement(declaration);
                            }
                        }
                        finally {
                            if(stream != null) stream.close();
                        }


                    }

                    @Override
                    public void close() throws Exception {
                        iter.close();
                    }
                };
            }
        };
    }

    @Override
    public TraitDefinition getTraitDef(int id) throws Exception {
        String className = traitDefIds.lookupId(id);

        try(var input = library.findClass(className)) {
            if(input == null) throw new InvalidModuleException();

            var classReader = new ClassReader(input);
            var traitLoadVisitor = new TraitLoadVisitor();
            classReader.accept(traitLoadVisitor, ClassReader.SKIP_CODE);
            return traitLoadVisitor.buildTraitDefinition();
        }
    }

    @Override
    public TraitReference getTraitRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassDefinition getClassDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassReference getClassRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public DataConstructorDefinition getDataConstructorDef(int id) throws Exception {
        throw new InvalidModuleException();
    }

    @Override
    public DataConstructorReference getDataConstructorRef(int id) throws Exception {
        throw new InvalidModuleException();
    }

    @Override
    public FunctionDefinition getFunctionDef(int id) throws Exception {
        throw new InvalidModuleException();
    }

    @Override
    public FunctionReference getFunctionRef(int id) throws Exception {
        throw new InvalidModuleException();
    }

    @Override
    public MethodDefinition getMethodDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public MethodReference getMethodRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassConstructorDefinition getClassConstructorDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassConstructorReference getClassConstructorRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    private final class LibraryNameVisitor implements JavaLibraryVisitor<String> {
        @Override
        public String visitModule(JavaModule module) {
            return JavaModule.idFromModuleName(module.getModuleName());
        }

        @Override
        public String visitClassPath(ClassPath classPath) {
            return ClassPath.classpathModuleId;
        }
    }


    private final class LibraryReferencesVisitor implements JavaLibraryVisitor<ExIterable<ModuleReference>> {
        @Override
        public ExIterable<ModuleReference> visitModule(JavaModule module) {
            return new ExIterable<>() {
                @NotNull
                @Override
                public ExIterator<ModuleReference> iterator() {
                    final Set<String> seenModules = new HashSet<>();
                    final Queue<String> remainingModules = new ArrayDeque<>(module.getRequires());

                    return new ExIterator<>() {
                        @Override
                        public ModuleReference next() throws Exception {
                            String moduleName;
                            do {
                                moduleName = remainingModules.poll();
                            } while (moduleName != null && seenModules.contains(moduleName));

                            if(moduleName == null) {
                                return null;
                            }

                            seenModules.add(moduleName);

                            var module = factory.loadByJavaModuleName(moduleName);
                            remainingModules.addAll(module.getRequires());

                            return ModuleReference.newBuilder()
                                    .setName(moduleName)
                                    .build();
                        }

                        @Override
                        public void close() throws Exception {}
                    };
                }
            };
        }

        @Override
        public ExIterable<ModuleReference> visitClassPath(ClassPath classPath) {
            return ExIterable.empty();
        }
    }

    private final class ClassFileLibraryVisitor implements JavaLibraryVisitor<JavaLibrary> {
        public ClassFileLibraryVisitor(String className) {
            this.className = className;
        }

        private final String className;

        @Override
        public JavaLibrary visitModule(JavaModule module) throws Exception {
            String pkg;
            int lastSlash = className.lastIndexOf('/');
            if(lastSlash >= 0) pkg = className.substring(lastSlash + 1);
            else pkg = className;

            if(module.exportsPackage(pkg)) {
                return library;
            }

            for(var ref : references().getReferencesList()) {
                var refModule = factory.loadByJavaModuleName(ref.getName());

                if(refModule.exportsPackage(pkg)) {
                    return refModule;
                }
            }

            return null;
        }

        @Override
        public JavaLibrary visitClassPath(ClassPath classPath) {
            return library;
        }
    }

    @FunctionalInterface
    private interface ExpressionSetup {
        void setup(Expression.Builder builder) throws Exception;
        
        default Expression toExpression() throws Exception {
            var builder = Expression.newBuilder();
            setup(builder);
            return builder.build();
        }
    }

    private String getPrimitiveClassName(char descriptor) {
        switch (descriptor) {
            case 'Z': return "Boolean";
            case 'C': return "Char";
            case 'B': return "Byte";
            case 'S': return "Short";
            case 'I': return "Int";
            case 'F': return "Float";
            case 'J': return "Long";
            case 'D': return "Double";
            case 'V': return "Void";
            default: return null;
        }
    }

    private abstract class TypeParserSignatureVisitor extends SignatureVisitor {

        public TypeParserSignatureVisitor(Map<String, Expression> typeParameters) {
            super(Constants.AsmVersion);
            this.typeParameters = typeParameters;
        }

        private final Map<String, Expression> typeParameters;

        private String className;

        protected abstract void visitExpression(ExpressionSetup expr);

        @Override
        public void visitBaseType(char descriptor) {
            String primitiveClassName = getPrimitiveClassName(descriptor);

            ExpressionSetup expressionSetup;
            if(primitiveClassName == null) expressionSetup = builder -> { throw new InvalidModuleException(); };
            else expressionSetup = getPrimitiveClass(primitiveClassName);

            visitExpression(expressionSetup);
        }

        private ExpressionSetup getPrimitiveClass(String name) {

            String className = primitiveClassPrefix + name;
            int classId = classRefIds.getId(className);

            return builder -> builder.setClassType(classId);
        }

        @Override
        public SignatureVisitor visitArrayType() {
            return new TypeParserSignatureVisitor(typeParameters) {
                @Override
                protected void visitExpression(ExpressionSetup expr) {
                    TypeParserSignatureVisitor.this.visitExpression(builder -> {
                        TypeParserSignatureVisitor.this.getPrimitiveClass("Array").setup(builder);
                        builder.addArgs(expr.toExpression());
                    });
                }
            };
        }

        @Override
        public void visitTypeVariable(String name) {
            var typeParameter = typeParameters.get(name);
            visitExpression(builder -> builder.mergeFrom(typeParameter));
        }

        @Override
        public void visitClassType(String name) {
            className = name;
        }

        @Override
        public void visitTypeArgument() {
            throw new NotImplementedError();
        }

        @Override
        public SignatureVisitor visitTypeArgument(char wildcard) {
            throw new NotImplementedError();
        }

        @Override
        public void visitInnerClassType(String name) {
            throw new NotImplementedError();
        }

        @Override
        public void visitEnd() {
            visitExpression(builder -> {
                if(className == null) throw new InvalidModuleException();

                var classLib = library.visit(new ClassFileLibraryVisitor(className));
                if(classLib == null) throw new InvalidModuleException();

                boolean isTrait;
                try(var stream = classLib.findClass(className)) {
                    if(stream == null) throw new InvalidModuleException();
                    var reader = new ClassReader(stream);
                    isTrait = (reader.getAccess() & Opcodes.ACC_INTERFACE) == Opcodes.ACC_INTERFACE;
                }

                if(isTrait) {
                    int id = (classLib == library ? traitDefIds : traitRefIds).getId(className);
                    builder.setTraitType(id);
                }
                else {
                    int id = (classLib == library ? classDefIds : classRefIds).getId(className);
                    builder.setClassType(id);
                }
            });
        }
    }

    private abstract class SigTypeParserSignatureVisitor extends SignatureVisitor {

        public SigTypeParserSignatureVisitor() {
            super(Constants.AsmVersion);
        }

        private String className;

        protected abstract void visitType(Delayed<SigType> expr);

        @Override
        public void visitBaseType(char descriptor) {
            visitType(() -> {
                String primitiveClassName = getPrimitiveClassName(descriptor);

                String className = primitiveClassPrefix + primitiveClassName;
                int classId = classRefIds.getId(className);

                return SigType.newBuilder().setClassType(
                    SigTypeClass.newBuilder().setClassId(classId).build()
                ).build();
            });
        }

        @Override
        public SignatureVisitor visitArrayType() {
            return new SigTypeParserSignatureVisitor() {
                @Override
                protected void visitType(Delayed<SigType> expr) {
                    SigTypeParserSignatureVisitor.this.visitType(() -> {


                        String className = primitiveClassPrefix + "Array";
                        int classId = classRefIds.getId(className);

                        var classInfo = SigTypeClass.newBuilder();
                        classInfo.setClassId(classId);
                        classInfo.addTypeArguments(expr.get());

                        return SigType.newBuilder().setClassType(classInfo).build();
                    });
                }
            };
        }

        @Override
        public void visitTypeVariable(String name) {
            visitType(() -> SigType.newBuilder().setEmpty(Empty.newBuilder().build()).build());
        }

        @Override
        public void visitClassType(String name) {
            className = name;
        }

        @Override
        public void visitTypeArgument() {
            throw new NotImplementedError();
        }

        @Override
        public SignatureVisitor visitTypeArgument(char wildcard) {
            throw new NotImplementedError();
        }

        @Override
        public void visitInnerClassType(String name) {
            throw new NotImplementedError();
        }

        @Override
        public void visitEnd() {
            visitType(() -> {
                if(className == null) throw new InvalidModuleException();

                var classLib = library.visit(new ClassFileLibraryVisitor(className));
                if(classLib == null) throw new InvalidModuleException();

                boolean isTrait;
                try(var stream = classLib.findClass(className)) {
                    if(stream == null) throw new InvalidModuleException();
                    var reader = new ClassReader(stream);
                    isTrait = (reader.getAccess() & Opcodes.ACC_INTERFACE) == Opcodes.ACC_INTERFACE;
                }

                var builder = SigType.newBuilder();

                List<SigType> typeArguments = List.of();

                if(isTrait) {
                    int id = (classLib == library ? traitDefIds : traitRefIds).getId(className);
                    builder.setTraitType(SigTypeTrait.newBuilder().setTraitId(id).addAllTypeArguments(typeArguments));
                }
                else {
                    int id = (classLib == library ? classDefIds : classRefIds).getId(className);
                    builder.setClassType(SigTypeClass.newBuilder().setClassId(id).addAllTypeArguments(typeArguments));
                }

                return builder.build();
            });
        }
    }




    private final static class TypeParamCounterVisitor extends SignatureVisitor {
        public TypeParamCounterVisitor() {
            super(Constants.AsmVersion);
        }

        public int typeParamCount = 0;

        @Override
        public void visitFormalTypeParameter(String name) {
            typeParamCount += 1;
        }
    }


    private @Nullable String readSignature(ClassReader classReader) {
        final class SignatureReaderVisitor extends ClassVisitor {
            public SignatureReaderVisitor() {
                super(Constants.AsmVersion);
            }

            public String signature;

            @Override
            public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
                this.signature = signature;
            }
        }

        var visitor = new SignatureReaderVisitor();
        classReader.accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        return visitor.signature;
    }

    private abstract class ClassFileLoadVisitor extends ClassVisitor {
        public ClassFileLoadVisitor() {
            super(Constants.AsmVersion);
        }

        protected int classAccess;
        protected String className;
        protected String classSignature;
        protected String classSuperName;
        protected String[] classInterfaces;
        protected final List<MethodMember> methods = new ArrayList<>();
        protected final List<MethodMember> staticMethods = new ArrayList<>();
        protected final List<MethodMember> constructors = new ArrayList<>();

        protected abstract MethodOwner instanceOwner();
        protected abstract MethodOwner staticOwner();
        protected abstract Map<String, Expression> typeParameters();

        @Override
        public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
            this.classAccess = access;
            this.className = name;
            this.classSignature = signature;
            this.classSuperName = superName;
            this.classInterfaces = interfaces;
        }

        @Override
        public FieldVisitor visitField(int access, String name, String descriptor, String signature, Object value) {
            return null;
        }

        @Override
        public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
            int id = methodDefIds.getId(new MethodIdentifier(className, name, descriptor));
            var parameterNames = new ArrayList<String>();

            AccessModifier accessModifier;
            if((access & Opcodes.ACC_PRIVATE) == Opcodes.ACC_PRIVATE) {
                accessModifier = AccessModifier.Private;
            }
            else if((access & Opcodes.ACC_PUBLIC) == Opcodes.ACC_PUBLIC) {
                accessModifier = AccessModifier.Public;
            }
            else if((access & Opcodes.ACC_PROTECTED) == Opcodes.ACC_PROTECTED) {
                accessModifier = AccessModifier.Protected;
            }
            else {
                accessModifier = AccessModifier.Internal;
            }

            var methodMember = MethodMember.newBuilder()
                .setId(id)
                .setAccessModifier(accessModifier)
                .build();

            if((access & Opcodes.ACC_STATIC) == Opcodes.ACC_STATIC) {
                staticMethods.add(methodMember);
            }
            else {
                methods.add(methodMember);
            }

            return new MethodVisitor(Constants.AsmVersion) {

                @Override
                public void visitParameter(String name, int access) {
                    parameterNames.add(name);
                }

                @Override
                public void visitEnd() {
                    methodDefs.put(id, () -> createMethodDef(access, name, signature, parameterNames));
                }
            };
        }

        protected MethodDefinition createMethodDef(int access, String name, String signature, List<String> parameterNames) throws Exception {
            var builder = MethodDefinition.newBuilder();
            builder.setOwner(((access & Opcodes.ACC_STATIC) == Opcodes.ACC_STATIC) ? staticOwner() : instanceOwner());
            builder.setName(MethodName.newBuilder().setNormal(name).build());
            builder.setFileId(getFileId(className));
            builder.setSignature(createMethodSig(signature, parameterNames));
            builder.setEffects(EffectInfo.newBuilder().setIsPure(false).build());
            builder.setIsVirtual(true);
            if((access & Opcodes.ACC_ABSTRACT) == Opcodes.ACC_ABSTRACT) builder.setIsAbstract(true);
            if((access & Opcodes.ACC_FINAL) == Opcodes.ACC_FINAL) builder.setIsFinal(true);
            return builder.build();
        }



        private MethodSignature createMethodSig(String signature, List<String> parameterNames) throws Exception {
            final class SigParserVisitor extends SignatureVisitor {
                public SigParserVisitor() {
                    super(Constants.AsmVersion);
                }

                private final List<String> typeParameterNames = new ArrayList<>();
                private final List<ExpressionSetup> typeParameterSubtypeBound = new ArrayList<>();
                private final Map<String, Expression> typeParameterMap = new HashMap<>(typeParameters());

                private final List<ExpressionSetup> parameterTypes = new ArrayList<>();
                private ExpressionSetup returnType;

                private String currentTypeParamName = null;

                private void completeLastTypeParameter() {
                    if(typeParameterNames.size() > typeParameterSubtypeBound.size()) {
                        typeParameterSubtypeBound.add(null);
                    }

                    if(!typeParameterNames.isEmpty()) {
                        // TODO: Add to type parameter map
                        throw new NotImplementedError();
                    }
                }

                @Override
                public void visitFormalTypeParameter(String name) {
                    completeLastTypeParameter();
                    typeParameterNames.add(name);
                }

                @Override
                public SignatureVisitor visitClassBound() {
                    return new TypeParserSignatureVisitor(typeParameterMap) {
                        @Override
                        protected void visitExpression(ExpressionSetup expr) {
                            typeParameterSubtypeBound.add(expr);
                        }
                    };
                }

                @Override
                public SignatureVisitor visitInterfaceBound() {
                    return new TypeParserSignatureVisitor(typeParameterMap) {
                        @Override
                        protected void visitExpression(ExpressionSetup expr) {
                            if(typeParameterNames.size() > typeParameterSubtypeBound.size()) {
                                typeParameterSubtypeBound.add(expr);
                            }
                            else {
                                final ExpressionSetup prev = typeParameterSubtypeBound.get(typeParameterSubtypeBound.size() - 1);
                                typeParameterSubtypeBound.set(typeParameterSubtypeBound.size() - 1, binding -> {
                                    binding.setUnionType(Empty.newBuilder().build());
                                    binding.addArgs(prev.toExpression());
                                    binding.addArgs(expr.toExpression());
                                });
                            }
                        }
                    };
                }

                @Override
                public SignatureVisitor visitParameterType() {
                    completeLastTypeParameter();

                    return new TypeParserSignatureVisitor(typeParameterMap) {
                        @Override
                        protected void visitExpression(ExpressionSetup expr) {
                            parameterTypes.add(expr);
                        }
                    };
                }

                @Override
                public SignatureVisitor visitReturnType() {
                    completeLastTypeParameter();

                    return new TypeParserSignatureVisitor(typeParameterMap) {
                        @Override
                        protected void visitExpression(ExpressionSetup expr) {
                            returnType = expr;
                        }
                    };
                }
            }

            var visitor = new SigParserVisitor();
            new SignatureReader(signature).accept(visitor);

            var methodSig = MethodSignature.newBuilder();

            if(!visitor.typeParameterNames.isEmpty()) {
                var typeParameters = Parameter.newBuilder();
                typeParameters.setStyle(ParameterStyle.Inferrable);
                typeParameters.setIsErased(true);

                for(int i = 0; i < visitor.typeParameterNames.size(); ++i) {

                    var bound = visitor.typeParameterSubtypeBound.get(i);

                    var typeN = TypeN.newBuilder();
                    typeN.setUniverse(UniverseExpr.newBuilder().setFixed(BigInt.newBuilder().setSigned32(1).build()).build());
                    if(bound != null) {
                        typeN.setSubtypeConstraint(bound.toExpression());
                    }

                    var paramType = Expression.newBuilder();
                    paramType.setTypeN(typeN);

                    typeParameters.addElements(
                        ParameterElement.newBuilder()
                            .setName(visitor.typeParameterNames.get(i))
                            .setParamType(paramType)
                            .build()
                    );
                }

                methodSig.addParameters(typeParameters);
            }

            var parameters = Parameter.newBuilder();
            parameters.setStyle(ParameterStyle.Normal);

            for(int i = 0; i < visitor.parameterTypes.size(); ++i) {
                var element = ParameterElement.newBuilder();
                if(!parameterNames.isEmpty()) element.setName(parameterNames.get(i));
                element.setParamType(visitor.parameterTypes.get(i).toExpression());
                parameters.addElements(element);
            }

            methodSig.addParameters(parameters);

            methodSig.setReturnType(visitor.returnType.toExpression());

            return methodSig.build();
        }

    }



    private final class TraitLoadVisitor extends ClassFileLoadVisitor {
        @Override
        protected MethodOwner instanceOwner() {
            throw new NotImplementedError();
        }

        @Override
        protected MethodOwner staticOwner() {
            throw new NotImplementedError();
        }

        @Override
        protected Map<String, Expression> typeParameters() {
            throw new NotImplementedError();
        }

        public TraitDefinition buildTraitDefinition() {
            throw new NotImplementedError();
        }
    }
}
