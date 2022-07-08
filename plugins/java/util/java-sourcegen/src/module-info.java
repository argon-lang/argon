module dev.argon.plugin.util.javasourcegen {
    exports dev.argon.plugin.util.javasourcegen;
    
    provides javax.annotation.processing.Processor
        with dev.argon.plugin.util.javasourcegen.OptionsProcessor;

    requires java.compiler;
    requires org.apache.commons.text;
    requires org.checkerframework.checker.qual;
}
