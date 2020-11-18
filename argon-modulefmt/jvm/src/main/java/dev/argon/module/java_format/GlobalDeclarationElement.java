package dev.argon.module.java_format;

public abstract class GlobalDeclarationElement {
    private GlobalDeclarationElement() {}

    public abstract <T> T visit(Visitor<T> visitor);

    public interface Visitor<T> {
        T visitTrait(GlobalDeclarationType traitElement);
        T visitClass(GlobalDeclarationType classElement);
        T visitDataConstructor(GlobalDeclarationType dataConstructorElement);
        T visitFunction(GlobalDeclarationFunction functionElement);
    }

    public static final class TraitElement extends GlobalDeclarationElement {
        public TraitElement(GlobalDeclarationType traitElement) {
            this.traitElement = traitElement;
        }

        public final GlobalDeclarationType traitElement;

        @Override
        public <T> T visit(Visitor<T> visitor) {
            return visitor.visitTrait(traitElement);
        }
    }

    public static final class ClassElement extends GlobalDeclarationElement {
        public ClassElement(GlobalDeclarationType classElement) {
            this.classElement = classElement;
        }

        public final GlobalDeclarationType classElement;

        @Override
        public <T> T visit(Visitor<T> visitor) {
            return visitor.visitClass(classElement);
        }
    }

    public static final class DataConstructorElement extends GlobalDeclarationElement {
        public DataConstructorElement(GlobalDeclarationType dataConstructorElement) {
            this.dataConstructorElement = dataConstructorElement;
        }

        public final GlobalDeclarationType dataConstructorElement;

        @Override
        public <T> T visit(Visitor<T> visitor) {
            return visitor.visitDataConstructor(dataConstructorElement);
        }
    }

    public static final class FunctionElement extends GlobalDeclarationElement {
        public FunctionElement(GlobalDeclarationFunction functionElement) {
            this.functionElement = functionElement;
        }

        public final GlobalDeclarationFunction functionElement;

        @Override
        public <T> T visit(Visitor<T> visitor) {
            return visitor.visitFunction(functionElement);
        }
    }
}
