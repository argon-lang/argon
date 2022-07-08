package dev.argon.plugin.api;

public interface Platform<E extends Exception, ExternFunction, ExternMethod, ExternClassCtor> {
  String id();
  String name();

  ExternCodec<E, ExternFunction> externFunctionCodec();
  ExternCodec<E, ExternMethod> externMethodCodec();
  ExternCodec<E, ExternClassCtor> externClassConstructorCodec();
}
