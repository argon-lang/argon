package dev.argon.plugin

import java.lang.reflect.Field
import dev.argon.options.*
import dev.argon.util.{*, given}
import scala.jdk.CollectionConverters.*
import java.io.IOException
import dev.argon.plugin.{api => japi}
import zio.*
import dev.argon.io.BinaryResource

final class JavaOptionHandler[E >: IOException, EX <: Exception, Options, OptionsBuilder](impl: japi.options.OptionHandler[EX, Options, OptionsBuilder])(using ErrorWrapper[E, EX], Runtime[Any]) extends OptionHandler[E, Options] {
  override type Builder = OptionsBuilder

  override def options: Set[OptionInfo[E, ?, Options, OptionsBuilder]] =
    impl.options.asScala
      .map(convertOptionInfo)
      .toSet

  private def convertOptionInfo[A](optionInfo: japi.options.OptionInfo[EX, A, Options, OptionsBuilder]): OptionInfo[E, A, Options, OptionsBuilder] =
    optionInfo match {
      case optionInfo: japi.options.OptionInfoValue[EX, A, Options, OptionsBuilder] =>
        new OptionInfoValue[E, A, Options, OptionsBuilder] {
          override val name: String = optionInfo.name
          override val description: String = optionInfo.description


          def addOptionValue(prev: Builder, value: String): Either[OptionsError.ParseError, Builder] =
            try {
              Right(optionInfo.addOptionValue(prev, value))
            }
            catch {
              case ex: Throwable if JavaErrorHandler.handleOptionsExceptions.isDefinedAt(ex) =>
                Left(JavaErrorHandler.handleOptionsExceptions(ex))
            }

          def getValue(options: Options): A =
            optionInfo.getValue(options)
          
        }

      case optionInfo: japi.options.OptionInfoResource[EX, A, Options, OptionsBuilder] =>
        new OptionInfoResource[E, A, Options, OptionsBuilder] {
          override val name: String = optionInfo.name
          override val description: String = optionInfo.description


          def addOptionValue(prev: Builder, value: BinaryResource[E]): Either[OptionsError.ParseError, Builder] =
            try {
              Right(optionInfo.addOptionValue(prev, new JavaBinaryResourceWrap[E, EX](value)))
            }
            catch {
              case ex: Throwable if JavaErrorHandler.handleOptionsExceptions.isDefinedAt(ex) =>
                Left(JavaErrorHandler.handleOptionsExceptions(ex))
            }
            

          def getValue(options: Options): A =
            optionInfo.getValue(options)
          
        }

      case optionInfo => throw new MatchError(optionInfo)
    }

  
  override def emptyBuilder: Builder = impl.createBuilder()
  override def build(builder: Builder): Either[OptionsError.MissingOption, Options] =
    try {
      Right(impl.build(builder))
    }
    catch {
      case ex: japi.options.MissingOptionException =>
        Left(OptionsError.MissingOption(Seq(ex.optionName)))
    }
    

}

object JavaOptionHandler {
  def apply[E >: IOException, EX <: Exception, Options](impl: japi.options.OptionHandler[EX, Options, ?])(using Runtime[Any], ErrorWrapper[E, EX]): OptionHandler[E, Options] =
    new JavaOptionHandler(impl)
}
