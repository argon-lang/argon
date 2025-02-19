package dev.argon.backend.metadata

import toml.Value

final case class BackendMetadata(
  backend: BackendBackendMetadata,
  options: BackendOptionsSchema = BackendOptionsSchema(),
) derives toml.Codec

final case class BackendBackendMetadata(
  `api-version`: String,
  name: String,
) derives toml.Codec

final case class BackendOptionsSchema(
  externs: Map[String, BackendOption] = Map.empty,
  codegen: Map[String, BackendOption] = Map.empty,
  output: Map[String, BackendOptionOutput] = Map.empty,
) derives toml.Codec

final case class BackendOption(
  `type`: OptionType,
  occurrence: OptionOccurrence,
) derives toml.Codec

final case class BackendOptionOutput(
  `type`: OutputType,
) derives toml.Codec


enum OptionType {
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

enum OptionOccurrence {
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

enum OutputType {
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

