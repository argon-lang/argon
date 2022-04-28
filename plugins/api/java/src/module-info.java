module dev.argon.plugin.api {
    exports dev.argon.plugin.api;
    exports dev.argon.plugin.api.options;
    exports dev.argon.plugin.api.resource;
    exports dev.argon.plugin.api.tube;
    exports dev.argon.plugin.api.util;

    requires transitive dev.argon.verilization.runtime;
    requires transitive org.checkerframework.checker.qual;
}
