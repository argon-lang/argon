package dev.argon.vm;

import java.io.IOException;
import java.util.List;

public final class Runtime {
  public Runtime(List<TubeLoader> tubeLoaders) throws IOException {
    tubes = new Tube[tubeLoaders.size()];
    for(int i = 0; i < tubeLoaders.size(); ++i) {
        tubes[i] = tubeLoaders.get(i).load(this, i);
    }

    this.classLoader = new RuntimeClassLoader(this.classLoader(), this);
  }

  private final Tube[] tubes;
  private final ClassLoader classLoader;
  
  public ClassLoader classLoader() {
    return classLoader;
  }

  public Tube getTube(int id) {
    return tubes[id];
  }

  public Tube getTube(Name name) {
    for(int i = 0; i < tubes.length; ++i) {
        var tube = tubes[i];
        if(tube.tubeName().equals(name)) {
            return tube;
        }
    }

    return null;
  }

}
