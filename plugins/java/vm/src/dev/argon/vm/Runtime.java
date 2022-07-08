package dev.argon.vm;

import java.util.Arrays;

public final class Runtime {
  public Runtime(String runtimeId, Tube[] tubes) {
    this.runtimeId = runtimeId;
    this.tubes = Arrays.copyOf(tubes, tubes.length);
    this.classLoader = new RuntimeClassLoader(this.classLoader(), this);
  }

  private final String runtimeId;
  private final Tube[] tubes;
  private final ClassLoader classLoader;
  
  public ClassLoader classLoader() {
    return classLoader;
  }

  public Tube getTube(int id) {
    return tubes[id];
  }

}
