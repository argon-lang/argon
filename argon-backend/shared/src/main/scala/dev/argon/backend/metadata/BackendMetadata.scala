package dev.argon.backend.metadata

import dev.argon.backend.scalaApi.metadata as scalaApi
import esexpr.Dictionary
import toml.Value

final case class BackendMetadata(
  backend: BackendBackendMetadata,
  options: BackendOptionsSchema = BackendOptionsSchema(),
  loaders: List[BackendLoaderOptions],
) derives toml.Codec
object BackendMetadata {
  def fromApi(backendMetadata: scalaApi.BackendMetadata): BackendMetadata =
    BackendMetadata(
      backend = BackendBackendMetadata.fromApi(backendMetadata.backend),
      options = BackendOptionsSchema.fromApi(backendMetadata.options),
      loaders = backendMetadata.loaders.map(BackendLoaderOptions.fromApi).toList,
    )

  def toApi(backendMetadata: BackendMetadata): scalaApi.BackendMetadata =
    scalaApi.BackendMetadata(
      backend = BackendBackendMetadata.toApi(backendMetadata.backend),
      options = BackendOptionsSchema.toApi(backendMetadata.options),
      loaders = backendMetadata.loaders.map(BackendLoaderOptions.toApi),
    )
}

final case class BackendSchema(
  backend: BackendBackendMetadata,
  options: BackendOptionsSchema = BackendOptionsSchema(),
  loaders: List[BackendLoaderOptions],
) derives toml.Codec {
  def toBackendMetadata: BackendMetadata =
    BackendMetadata(
      backend = backend,
      options = options,
      loaders = loaders,
    )
}

final case class BackendBackendMetadata(
  `api-version`: String,
  name: String,
) derives toml.Codec
object BackendBackendMetadata {
  def fromApi(backendBackendMetadata: scalaApi.BackendBackendMetadata): BackendBackendMetadata =
    BackendBackendMetadata(
      `api-version` = backendBackendMetadata.apiVersion,
      name = backendBackendMetadata.name,
    )

  def toApi(backendBackendMetadata: BackendBackendMetadata): scalaApi.BackendBackendMetadata =
    scalaApi.BackendBackendMetadata(
      apiVersion = backendBackendMetadata.`api-version`,
      name = backendBackendMetadata.name,
    )
}

final case class BackendOptionsSchema(
  tube: Map[String, BackendOption] = Map.empty,
  codegen: Map[String, BackendOption] = Map.empty,
  output: Map[String, BackendOptionOutput] = Map.empty,
) derives toml.Codec
object BackendOptionsSchema {
  def fromApi(backendOptionsSchema: scalaApi.BackendOptionsSchema): BackendOptionsSchema =
    BackendOptionsSchema(
      tube = backendOptionsSchema.tube.dict.view.mapValues(BackendOption.fromApi).toMap,
      codegen = backendOptionsSchema.codegen.dict.view.mapValues(BackendOption.fromApi).toMap,
      output = backendOptionsSchema.output.dict.view.mapValues(BackendOptionOutput.fromApi).toMap,
    )

  def toApi(backendOptionsSchema: BackendOptionsSchema): scalaApi.BackendOptionsSchema =
    scalaApi.BackendOptionsSchema(
      tube = Dictionary(backendOptionsSchema.tube.view.mapValues(BackendOption.toApi).toMap),
      codegen = Dictionary(backendOptionsSchema.codegen.view.mapValues(BackendOption.toApi).toMap),
      output = Dictionary(backendOptionsSchema.output.view.mapValues(BackendOptionOutput.toApi).toMap),
    )
}

final case class BackendOption(
  `type`: OptionType,
  description: String,
  occurrence: OptionOccurrence = OptionOccurrence.Default,
) derives toml.Codec
object BackendOption {
  def fromApi(backendOption: scalaApi.BackendOption): BackendOption =
    BackendOption(
      `type` = OptionType.fromApi(backendOption.`type`),
      description = backendOption.description,
      occurrence = OptionOccurrence.fromApi(backendOption.occurrence),
    )

  def toApi(backendOption: BackendOption): scalaApi.BackendOption =
    scalaApi.BackendOption(
      `type` = OptionType.toApi(backendOption.`type`),
      description = backendOption.description,
      occurrence = OptionOccurrence.toApi(backendOption.occurrence),
    )
}

final case class BackendOptionOutput(
  `type`: OutputType,
  description: String,
) derives toml.Codec
object BackendOptionOutput {
  def fromApi(backendOptionOutput: scalaApi.BackendOptionOutput): BackendOptionOutput =
    BackendOptionOutput(
      `type` = OutputType.fromApi(backendOptionOutput.`type`),
      description = backendOptionOutput.description,
    )

  def toApi(backendOptionOutput: BackendOptionOutput): scalaApi.BackendOptionOutput =
    scalaApi.BackendOptionOutput(
      `type` = OutputType.toApi(backendOptionOutput.`type`),
      description = backendOptionOutput.description,
    )
}


enum OptionType derives CanEqual {
  case String
  case Bool
  case BinaryResource
  case DirectoryResource
}

object OptionType {
  given toml.Codec[OptionType]:
    import toml.Codec.Defaults
    import toml.Parse.{Address, Message}

    override def apply(value: Value, defaults: Defaults, index: Int): Either[(Address, Message), OptionType] =
      value match {
        case Value.Str("string") => Right(String)
        case Value.Str("bool") => Right(Bool)
        case Value.Str("binary-resource") => Right(BinaryResource)
        case Value.Str("directory-resource") => Right(DirectoryResource)
        case _ => Left((List.empty, s"One of the option type strings was expected, $value provided"))
      }
  end given

  def fromApi(optionType: scalaApi.OptionType): OptionType =
    optionType match {
      case scalaApi.OptionType.String => String
      case scalaApi.OptionType.Bool => Bool
      case scalaApi.OptionType.BinaryResource => BinaryResource
      case scalaApi.OptionType.DirectoryResource => DirectoryResource
    }

  def toApi(optionType: OptionType): scalaApi.OptionType =
    optionType match {
      case String => scalaApi.OptionType.String
      case Bool => scalaApi.OptionType.Bool
      case BinaryResource => scalaApi.OptionType.BinaryResource
      case DirectoryResource => scalaApi.OptionType.DirectoryResource
    }
}

enum OptionOccurrence derives CanEqual {
  case Default
  case Optional
  case Required
  case Many
  case ManyRequired
}

object OptionOccurrence {
  given toml.Codec[OptionOccurrence]:
    import toml.Codec.Defaults
    import toml.Parse.{Address, Message}

    override def apply(value: Value, defaults: Defaults, index: Int): Either[(Address, Message), OptionOccurrence] =
      value match {
        case Value.Str("optional") => Right(Optional)
        case Value.Str("required") => Right(Required)
        case Value.Str("many") => Right(Many)
        case Value.Str("many-required") => Right(ManyRequired)
        case _ => Left((List.empty, s"One of the option occurrence strings was expected, $value provided"))
      }
  end given

  def fromApi(optionOccurrence: scalaApi.OptionOccurrence): OptionOccurrence =
    optionOccurrence match {
      case scalaApi.OptionOccurrence.Default => Default
      case scalaApi.OptionOccurrence.Optional => Optional
      case scalaApi.OptionOccurrence.Required => Required
      case scalaApi.OptionOccurrence.Many => Many
      case scalaApi.OptionOccurrence.ManyRequired => ManyRequired
    }

  def toApi(optionOccurrence: OptionOccurrence): scalaApi.OptionOccurrence =
    optionOccurrence match {
      case Default => scalaApi.OptionOccurrence.Default
      case Optional => scalaApi.OptionOccurrence.Optional
      case Required => scalaApi.OptionOccurrence.Required
      case Many => scalaApi.OptionOccurrence.Many
      case ManyRequired => scalaApi.OptionOccurrence.ManyRequired
    }
}

enum OutputType derives CanEqual {
  case BinaryResource
  case DirectoryResource
}

object OutputType {
  given toml.Codec[OutputType]:
    import toml.Codec.Defaults
    import toml.Parse.{Address, Message}

    override def apply(value: Value, defaults: Defaults, index: Int): Either[(Address, Message), OutputType] =
      value match {
        case Value.Str("binary-resource") => Right(BinaryResource)
        case Value.Str("directory-resource") => Right(DirectoryResource)
        case _ => Left((List.empty, s"One of the output type strings was expected, $value provided"))
      }
  end given
  
  def fromApi(optionType: scalaApi.OutputType): OutputType =
    optionType match {
      case scalaApi.OutputType.BinaryResource => BinaryResource
      case scalaApi.OutputType.DirectoryResource => DirectoryResource
    }

  def toApi(outputType: OutputType): scalaApi.OutputType =
    outputType match {
      case BinaryResource => scalaApi.OutputType.BinaryResource
      case DirectoryResource => scalaApi.OutputType.DirectoryResource
    }
}

sealed trait BackendLoaderOptions

object BackendLoaderOptions {
  def fromApi(backendLoaderOptions: scalaApi.BackendLoaderOptions): BackendLoaderOptions =
    backendLoaderOptions match {
      case scalaApi.BackendLoaderOptions.Js(jsOptions) =>
        JSLoaderOptions(
          `import-path` = jsOptions.importPath,
          `export-name` = jsOptions.exportName,
        )
    }

  def toApi(backendLoaderOptions: BackendLoaderOptions): scalaApi.BackendLoaderOptions =
    backendLoaderOptions match {
      case JSLoaderOptions(importPath, exportName) =>
        scalaApi.BackendLoaderOptions.Js(
          scalaApi.JsLoaderOptions(
            importPath = importPath,
            exportName = exportName,
          )
        )
    }

  final case class JSLoaderOptions(
    `import-path`: String,
    `export-name`: String,
  ) extends BackendLoaderOptions derives toml.Codec

  given toml.Codec[BackendLoaderOptions]:
    override def apply(value: Value, defaults: toml.Codec.Defaults, index: Int): Either[toml.Parse.Error, BackendLoaderOptions] =
      value match {
        case Value.Tbl(tbl) =>
          tbl.get("api") match {
            case Some(Value.Str(loaderType)) =>
              val withoutType = Value.Tbl(tbl.removed("api"))

              loaderType match {
                case "js" => summon[toml.Codec[JSLoaderOptions]].apply(withoutType, defaults, index)
                case _ => Left((List.empty, s"Unknown loader api: $loaderType"))
              }

            case Some(loaderType) =>
              Left((List("api"), s"Expected string, found ${loaderType}"))

            case None =>
              Left((List.empty, "Expected table with 'api' field"))
          }

        case _ =>
          Left((List.empty, s"Expected table, found $value"))
      }
  end given

}


