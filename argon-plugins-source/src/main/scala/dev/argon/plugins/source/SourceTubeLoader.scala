package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.io.{BinaryResource, DirectoryEntry, DirectoryResource, ResourceFactory}
import dev.argon.parser.IdentifierExpr
import dev.argon.parser.tubespec.{ModulePatternMapping, ModulePatternSegment}
import dev.argon.plugin.*
import dev.argon.options.*
import dev.argon.util.toml.Toml
import dev.argon.util.{*, given}

import java.io.IOException
import zio.*
import zio.stream.*

final class SourceTubeLoader[R <: SourceEnv, E >: SourceError, TPlugin <: SourcePlugin[R, E, ?]](val plugin: TPlugin) extends TubeLoader[R, E, TPlugin] {
  override type LibOptions = SourceLibOptions[R, E, plugin.Options]
  import plugin.optionDecoder

  override def libOptionDecoder: OptionDecoder[R, E, LibOptions] =
    summon[OptionDecoder[R, E, LibOptions]]
  
  def load
  (context: Context { type Env = R; type Error = E; type Options = plugin.Options })
  (tubeImporter: TubeImporter & HasContext[context.type])
  (libOptions: SourceLibOptions[context.Env, context.Error, context.Options])
  : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]] =
    for
      mappings <- libOptions.spec.tubeSpec.runCollect
      _ <- ZIO.logTrace("Getting source code files")
      sourceCode <- getSourceCode(context)(mappings)(Seq(), libOptions.sources).runCollect
      _ <- ZIO.logDebug(s"Found ${sourceCode.size} source files")

      tubeName = TubeName(libOptions.name)

      tube <- SourceTube.make(context, tubeImporter, tubeName, libOptions.platforms, sourceCode.toMap)
    yield tube


  private enum FileNameTemplate {
    case Variable(prefix: String, name: IdentifierExpr, suffix: String)
    case Literal(path: String)
  }

  private def getSourceCode
  (context: Context { type Error >: SourceError })
  (patterns: Seq[ModulePatternMapping])
  (path: Seq[String], resource: DirectoryResource[context.Env, context.Error, ArgonSourceCodeResource])
  : ZStream[context.Env, context.Error, (ModulePath, ArgonSourceCodeResource[context.Env, context.Error])] =
    resource.contents.flatMap {
      case DirectoryEntry.Subdirectory(name, resource) => getSourceCode(context)(patterns)(path :+ name, resource)
      case DirectoryEntry.File(name, resource) =>
        ZStream.fromZIOOption(
          ZIO.fromEither(getModulePath((path :+ name).toList)(patterns))
            .asSomeError
            .flatMap {
              case Some(modulePath) =>
                ZIO.logTrace(s"Found input file ${(path :+ name).mkString("/")}. Mapped to module ${modulePath}") *>
                ZIO.succeed(modulePath -> resource)
              case None =>
                ZIO.logDebug(s"Skipping input file ${(path :+ name).mkString("/")} as it was not mapped to a module") *>
                ZIO.fail(None)
            }
        )
    }


  private def getModulePath(path: List[String])(patterns: Seq[ModulePatternMapping]): Either[CompError, Option[ModulePath]] =
    patterns match {
      case head +: tail =>
        extractModulePath(path)(head) match {
          case Right(None) => getModulePath(path)(tail)
          case result => result
        }

      case _ => Right(None)
    }

  private def extractModulePath(path: List[String])(pattern: ModulePatternMapping): Either[CompError, Option[ModulePath]] =
    buildTemplate("", pattern.fileNameTemplate.parts.toList).flatMap { template =>
      matchFileName(pattern.module, Map.empty)(path)(template).flatMap {
        case Some(varValues) => resolveModuleName(pattern.module, varValues).map { parts => Some(ModulePath(parts)) }
        case None => Right(None)
      }
    }


  private def buildTemplate(prefix: String, template: List[StringToken.Part]): Either[CompError, List[FileNameTemplate]] =
    template match {
      case Nil if prefix.isEmpty => Right(Nil)

      case Nil => Right(prefix.split("/", -1).nn.map(part => FileNameTemplate.Literal(part.nn)).toList)

      case StringToken.StringPart(WithLocation(s, _)) :: tail =>
        (prefix + s).split("/", -1).nn.toSeq match {
          case init :+ last =>
            val initItems = init
              .iterator
              .map(_.nn)
              .filter(_.nonEmpty)
              .map(FileNameTemplate.Literal.apply)
              .toList
            buildTemplate(last.nn, tail).map { initItems ++ _ }

          case _ => buildTemplate(prefix + s, tail)
        }

      case (expr: StringToken.ExprPart) :: Nil =>
        buildTemplateExpr(prefix, expr, "").map(List(_))

      case (expr @ StringToken.ExprPart(_, _)) ::
            StringToken.StringPart(WithLocation(suffix1, suffix1Loc)) ::
            StringToken.StringPart(WithLocation(suffix2, suffix2Loc)) :: tail =>
        buildTemplate(prefix, expr :: StringToken.StringPart(WithLocation(suffix1 + suffix2, Location.merge(suffix1Loc, suffix2Loc))) :: tail)


      case (expr: StringToken.ExprPart) ::
            StringToken.StringPart(WithLocation(suffix, suffixLocation)) ::
            tail =>
        suffix.split("/", -1).nn.toList match {
          case suffixHead :: (suffixTail @ _ :: _) =>
            for
              current <- buildTemplateExpr(prefix, expr, suffixHead.nn)
              next <- buildTemplate(suffixTail.mkString("/"), tail)
            yield current :: next

          case _ =>
            if tail.nonEmpty then
              Left(DiagnosticError.SpecOneVariablePerTemplateSegment())
            else
              for
                current <- buildTemplateExpr(prefix, expr, suffix)
              yield List(current)


        }

      case StringToken.ExprPart(_, _) :: StringToken.ExprPart(_, _) :: _ =>
        Left(DiagnosticError.SpecOneVariablePerTemplateSegment())
    }

  private def buildTemplateExpr(prefix: String, expr: StringToken.ExprPart, suffix: String): Either[CompError, FileNameTemplate] =
    expr match {
      case StringToken.ExprPart(None, WithLocation(id: IdentifierExpr, _)) =>
        Right(FileNameTemplate.Variable(prefix, id, suffix))

      case _ =>
        Left(DiagnosticError.SpecFileNameTemplateMustBeIdentifier())
    }


  private def matchFileName(pattern: Seq[ModulePatternSegment], vars: Map[IdentifierExpr, Seq[String]])(path: List[String])(template: List[FileNameTemplate]): Either[CompError, Option[Map[IdentifierExpr, Seq[String]]]] =
    (path, template) match {
      case (Nil, Nil) => Right(Some(vars))

      case (Nil, FileNameTemplate.Variable("", varName, "") :: Nil) if isMultiPartGlob(pattern)(varName) =>
        Right(Some(vars + (varName -> Seq())))

      case (Nil, _) => Right(None)
      case (_, Nil) => Right(None)

      case (pathHead :: pathTail, FileNameTemplate.Literal(patternPath) :: templateTail) if pathHead == patternPath =>
        matchFileName(pattern, vars)(pathTail)(templateTail)

      case (_ :: _, FileNameTemplate.Literal(_) :: _) => Right(None)

      case (pathHead :: pathTail, FileNameTemplate.Variable(prefix, varName, suffix) :: templateTail) =>
        vars.get(varName) match {
          case Some(Seq(seg)) => matchFileName(pattern, vars)(path)(FileNameTemplate.Literal(prefix + seg + suffix) :: templateTail)
          case Some(segs) if prefix.isEmpty && suffix.isEmpty => matchFileName(pattern, vars)(path)(segs.map(FileNameTemplate.Literal.apply).toList ++ templateTail)
          case Some(_) => Left(DiagnosticError.SpecMultiPartGlobUsedWithPrefixSuffix())
          case None =>
            if isMultiPartGlob(pattern)(varName) then
              if prefix.nonEmpty || suffix.nonEmpty then
                Left(DiagnosticError.SpecMultiPartGlobUsedWithPrefixSuffix())
              else
                def matchGlob(globSegs: Seq[String])(path: List[String]): Either[CompError, Option[Map[IdentifierExpr, Seq[String]]]] =
                  matchFileName(pattern, vars + (varName -> globSegs))(path)(templateTail) match {
                    case Right(None) =>
                      path match {
                        case pathHead :: pathTail => matchGlob(globSegs :+ pathHead)(pathTail)
                        case Nil => Right(None)
                      }

                    case value =>
                      value
                  }
                matchGlob(Seq())(path)
              end if
            else
              if pathHead.length > prefix.length + suffix.length && pathHead.startsWith(prefix) && pathHead.endsWith(suffix) then
                val value = pathHead.substring(prefix.length, pathHead.length - suffix.length).nn
                matchFileName(pattern, vars + (varName -> Seq(value)))(pathTail)(templateTail)
              else
                Right(None)
              end if
            end if
        }


    }

  private def isMultiPartGlob(pattern: Seq[ModulePatternSegment])(name: IdentifierExpr): Boolean =
    pattern.collectFirst {
      case ModulePatternSegment.Star(`name`) => false
      case ModulePatternSegment.DoubleStar(`name`) => true
    }.getOrElse(false)

  private def resolveModuleName(pattern: Seq[ModulePatternSegment], vars: Map[IdentifierExpr, Seq[String]]): Either[CompError, Seq[String]] =
    pattern
      .traverse {
        case ModulePatternSegment.Named(name) => Right(Seq(name)) : Either[CompError, Seq[String]]
        case ModulePatternSegment.Star(boundName) => vars.get(boundName).toRight { DiagnosticError.SpecUndefinedVariable() }
        case ModulePatternSegment.DoubleStar(boundName) => vars.get(boundName).toRight { DiagnosticError.SpecUndefinedVariable() }
      }
      .map { _.flatten }

}
