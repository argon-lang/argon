package dev.argon.module

object ModuleFormatVersion {
  val currentVersion: Int = ArgonModuleProto.currentFormatVersion.get(Metadata.scalaDescriptor.getOptions).get
}
