package dev.argon.plugin

import dev.argon.util.{*, given}
import dev.argon.util.xml.*
import scala.util.Try
import dev.argon.plugin.PluginSpecification.PluginExecutor

object PluginXml {
  def parse(xml: Element): Either[PluginXmlException, PluginSpecification] =
    for {
      id <- xml.attribute(Name("id")).toRight { PluginNameMissingException() }
      name <- xml.attribute(Name("name")).toRight { PluginNameMissingException() }
      version <- xml.attribute(Name("version")).toRight { PluginVersionMissingException() }
      apiVersion <- parseAPIVersion(xml)
      options <- parseOptions(xml)
      executors <- parseExecutors(xml)
      resources <- parseResourceTypes(xml)
      backends <- parseBackends(xml)
      tubeLoaders <- parseTubeLoaders(xml)
      resourceLoaders <- parseResourceLoaders(xml)
      buildOutputExecutors <- parseBuildOutputExecutor(xml)
    } yield PluginSpecification(
      id = id.value,
      name = name.value,
      version = version.value,
      apiVersion = apiVersion,
      options = options,
      executors = executors,
      resources = resources,
      backends = backends,
      tubeLoaders = tubeLoaders,
      resourceLoaders = resourceLoaders,
      buildOutputExecutors = buildOutputExecutors,
    )

  private def parseAPIVersion(xml: Element): Either[PluginXmlException, PluginSpecification.APIVersion] = (
    for {
      apiVersion <- xml.child(Name("api-version"))
      major <- xml.child(Name("major"))
      majorInt <- Try { BigInt(major.textContent) }.toOption
      minor <- xml.child(Name("minor"))
      minorInt <- Try { BigInt(minor.textContent) }.toOption
    } yield PluginSpecification.APIVersion(majorInt, minorInt)
  ).toRight { PluginAPIVersionInvalidException() }




  private def parseOptions(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.PluginOptionDefinition]] =
    xml.child(Name("options")).toSeq
      .flatMap { options =>
        options.childrenByTag(Name("option"))
      }
      .traverse(parseOption)

  private def parseOption(opt: Element): Either[PluginXmlException, PluginSpecification.PluginOptionDefinition] =
    for {
      name <- opt.child(Name("name")).toRight { PluginOptionInvalidException("Missing name of option") }
      description <- opt.child(Name("description")).toRight { PluginOptionInvalidException("Missing description of option") }
      optionType <- opt.child(Name("type")).toRight { PluginOptionInvalidException("Missing type of option") }
      optionTypeParsed <- parseOptionType(optionType).toRight { PluginOptionInvalidException("Invalid option type") }
      defaultValue <- opt.child(Name("default")).traverse(parseOptionValue(optionTypeParsed)).toRight { PluginOptionInvalidException("Invalid default value of option") }
    } yield PluginSpecification.PluginOptionDefinition(
      name = name.textContent,
      description = description.textContent,
      optionType = optionTypeParsed,
      defaultValue = defaultValue,
    )

  private def parseOptionScalarType(opt: Element): Option[PluginSpecification.PluginOptionScalarType[_]] =
    opt.children.toList match {
      case (n: Element) :: Nil if n.name == Name("string") => Some(PluginSpecification.PluginOptionType.StringType)
      case (n: Element) :: Nil if n.name == Name("single-file") => Some(PluginSpecification.PluginOptionType.SingleFileType)
      case _ => None
    }


  private def parseOptionType(opt: Element): Option[PluginSpecification.PluginOptionType[_]] =
    opt.children match {
      case Seq(n: Element) if n.name == Name("file-list") => Some(PluginSpecification.PluginOptionType.FileListType)
      case Seq(n: Element) if n.name == Name("option") =>
        parseOptionScalarType(n).map(PluginSpecification.PluginOptionType.OptionType.apply)

      case Seq(n: Element) if n.name == Name("list") =>
        parseOptionScalarType(n).map(PluginSpecification.PluginOptionType.ListType.apply)

      case _ => parseOptionScalarType(opt)
    }

  private def parseOptionValue[T](optionType: PluginSpecification.PluginOptionType[T])(value: Element): Option[T] =
    optionType match {
      case PluginSpecification.PluginOptionType.StringType | PluginSpecification.PluginOptionType.SingleFileType =>
        Some(value.textContent)

      case PluginSpecification.PluginOptionType.FileListType =>
        value.childrenByTag(Name("value")).traverse(parseOptionValue(PluginSpecification.PluginOptionType.SingleFileType))

      case optionType: PluginSpecification.PluginOptionType.OptionType[elemType] =>
        value.child(Name("value")).traverse(parseOptionValue(optionType.t))
        
      case optionType: (PluginSpecification.PluginOptionType.ListType[elemType]) =>
        value.childrenByTag(Name("value")).traverse(parseOptionValue(optionType.t))
    }

  
  private def parseExecutors(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.PluginExecutor]] =
    xml.child(Name("executors")).toSeq
      .flatMap(_.children)
      .collect {
        case exec: Element => exec
      }
      .traverse { exec =>
        exec.name.name match {
          case "command-line-rpc" =>
            val command = exec.childrenByTag(Name("command"))
            (
              for {
                workDir <- exec.child(Name("working-directory"))
              } yield PluginSpecification.PluginExecutor.CommandLineRPC(
                command = command.map(_.textContent),
                workingDirectory = workDir.textContent,
              )
            ).toRight { PluginXmlException("Invalid RPC executor") }

          case "java" =>
            val jars = exec.childrenByTag(Name("jar")).map(_.textContent)

            Right(PluginSpecification.PluginExecutor.JavaAPI(jars))
          case "javascript" =>
            (
              for {
                modulePath <- exec.child(Name("module"))
                exportName <- exec.child(Name("export"))
              } yield PluginSpecification.PluginExecutor.JavaScriptAPI(
                modulePath = modulePath.textContent,
                exportName = exportName.textContent,
              )
            ).toRight { PluginXmlException("Invalid JavaScript executor") }

          case executorType =>
            Left(PluginXmlException(s"Unknown executor type: $executorType"))
        }
      }

  private def parseResourceTypes(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.ResourceTypeDefinition]] =
    xml.child(Name("resource-types")).toSeq
      .flatMap(_.childrenByTag(Name("resource")))
      .traverse { res =>
        for {
          extendsRes <- res.attribute(Name("extends"))
        } yield PluginSpecification.ResourceTypeDefinition(
          name = res.textContent,
          extendsResource = extendsRes.value,
        )
      }
      .toRight { PluginXmlException("Invalid resource type") }

  private def parseBackends(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.Backend]] =
    xml.childrenByTag(Name("backend"))
      .traverse { backend =>
        for {
          name <- backend.child(Name("name"))
          outputs <- backend.childrenByTag(Name("output"))
            .traverse { output =>
              for {
                name <- output.child(Name("name"))
                resourceType <- output.child(Name("resource"))
              } yield PluginSpecification.BackendOutput(
                name = name.textContent,
                resourceType = resourceType.textContent,
              )
            }
        } yield PluginSpecification.Backend(
          name = name.textContent,
          output = outputs,
        )
      }
      .toRight { PluginXmlException("Invalid backend") }

  private def parseTubeLoaders(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.TubeLoader]] =
    xml.childrenByTag(Name("tube-loader"))
      .traverse { loader =>
        for {
          name <- loader.child(Name("name"))
          ext = loader.childrenByTag(Name("extension"))
          res <- loader.child(Name("resource"))
        } yield PluginSpecification.TubeLoader(
          name = name.textContent,
          extensions = ext.map(_.textContent),
          resourceType = res.textContent,
        )
      }
      .toRight { PluginXmlException("Invalid tube loader") }

  private def parseResourceLoaders(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.ResourceLoader]] =
    xml.childrenByTag(Name("resource-loader"))
      .traverse { loader =>
        for {
          name <- loader.child(Name("name"))
          res <- loader.child(Name("resource"))
        } yield PluginSpecification.ResourceLoader(
          name = name.textContent,
          resourceType = res.textContent,
        )
      }
      .toRight { PluginXmlException("Invalid resource loader") }

  private def parseBuildOutputExecutor(xml: Element): Either[PluginXmlException, Seq[PluginSpecification.BuildOutputExecutor]] =
    xml.childrenByTag(Name("build-output-executor"))
      .traverse { loader =>
        for {
          name <- loader.child(Name("name"))
          res <- loader.child(Name("resource"))
        } yield PluginSpecification.BuildOutputExecutor(
          name = name.textContent,
          resourceType = res.textContent,
        )
      }
      .toRight { PluginXmlException("Invalid resource loader") }


}
