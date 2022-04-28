package dev.argon.plugin

import PluginSpecification.*

final case class PluginSpecification
(
  id: String,
  name: String,
  version: String,
  apiVersion: APIVersion,
  options: Seq[PluginOptionDefinition],
  executors: Seq[PluginExecutor],
  resources: Seq[ResourceTypeDefinition],
  backends: Seq[Backend],
  tubeLoaders: Seq[TubeLoader],
  resourceLoaders: Seq[ResourceLoader],
  buildOutputExecutors: Seq[BuildOutputExecutor],
)

object PluginSpecification {

  final case class APIVersion(major: BigInt, minor: BigInt)

  sealed trait PluginOptionType[T] {
    type Value = T
  }

  sealed trait PluginOptionScalarType[T] extends PluginOptionType[T]
  object PluginOptionType {
    given canEqualInstance[T]: CanEqual[PluginOptionType[T], PluginOptionType[T]] = CanEqual.derived

    case object StringType extends PluginOptionScalarType[String]
    case object SingleFileType extends PluginOptionScalarType[String]
    case object FileListType extends PluginOptionType[Seq[String]]
    final case class OptionType[Elem](t: PluginOptionScalarType[Elem]) extends PluginOptionType[Option[Elem]]
    final case class ListType[Elem](t: PluginOptionScalarType[Elem]) extends PluginOptionType[Seq[Elem]]
  }

  final class PluginOptionDefinition
  (
    val name: String,
    val description: String,
    val optionType: PluginOptionType[_],
    val defaultValue: Option[optionType.Value],
  )

  enum PluginExecutor {
    case CommandLineRPC(command: Seq[String], workingDirectory: String)
    case JavaAPI(jars: Seq[String])
    case JavaScriptAPI(modulePath: String, exportName: String)
  }

  final case class ResourceTypeDefinition(name: String, extendsResource: String)

  final case class Backend(name: String, output: Seq[BackendOutput])

  final case class BackendOutput(name: String, resourceType: String)

  final case class TubeLoader(name: String, extensions: Seq[String], resourceType: String)

  final case class ResourceLoader(name: String, resourceType: String)

  final case class BuildOutputExecutor(name: String, resourceType: String)

}
