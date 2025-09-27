package dev.argon.backend.metadata

import toml.Value

final case class BackendMetadata(
  backend: BackendBackendMetadata,
  options: BackendOptionsSchema = BackendOptionsSchema(),
) derives toml.Codec

final case class BackendSchema(
  backend: BackendBackendMetadata,
  options: BackendOptionsSchema = BackendOptionsSchema(),
  loaders: List[BackendLoaderOptions],
) derives toml.Codec {
  def toBackendMetadata: BackendMetadata =
    BackendMetadata(
      backend = backend,
      options = options,
    )
}

final case class BackendBackendMetadata(
  `api-version`: String,
  name: String,
) derives toml.Codec

final case class BackendOptionsSchema(
  tube: Map[String, BackendOption] = Map.empty,
  codegen: Map[String, BackendOption] = Map.empty,
  output: Map[String, BackendOptionOutput] = Map.empty,
) derives toml.Codec

final case class BackendOption(
  `type`: OptionType,
  description: String,
  occurrence: OptionOccurrence = OptionOccurrence.Default,
) derives toml.Codec

final case class BackendOptionOutput(
  `type`: OutputType,
  description: String,
) derives toml.Codec


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
}

sealed trait BackendLoaderOptions

object BackendLoaderOptions {
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


