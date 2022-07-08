package dev.argon.plugin.api.resource;

public sealed abstract class Resource<E extends Exception> permits DirectoryResource, BinaryResource {
  
}
