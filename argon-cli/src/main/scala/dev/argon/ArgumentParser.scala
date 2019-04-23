package dev.argon

import scalaz._
import Scalaz._

sealed trait ArgumentParser[A] {
  def parse(args: List[String]): CommandParseResult[(A, List[String])]
}

import scala.collection.immutable.Seq

trait ArgumentValueParser[A] {
  def parse(args: List[String]): Option[(A, List[String])]
}

final case class SimpleValueParser[A]
(
  name: String,
  shortName: String,
  description: String,
  value: ArgumentValueParser[A],
) extends ArgumentParser[A] {
  override def parse(args: List[String]): CommandParseResult[(A, List[String])] = args match {
    case switch :: tail if (name.nonEmpty && switch === "--" + name) || (shortName.nonEmpty && switch === "-" + shortName) =>
      value.parse(tail) match {
        case Some(value) => CommandParseResult.Value(value)
        case None => CommandParseResult.Error
      }

    case _ =>
      CommandParseResult.Skip
  }
}

final case class RestValueParser[A]
(
  name: String,
  description: String,
  value: ArgumentValueParser[A],
) extends ArgumentParser[A] {
  override def parse(args: List[String]): CommandParseResult[(A, List[String])] =
    value.parse(args) match {
      case Some(value) => CommandParseResult.Value(value)
      case None => CommandParseResult.Error
    }
}

final case class SubCommandMultiParser[A]
(
  commands: Seq[SubCommandParser[A]]
) extends ArgumentParser[A] {
  override def parse(args: List[String]): CommandParseResult[(A, List[String])] =
    commands.foldLeft[CommandParseResult[(A, List[String])]](CommandParseResult.Skip) { (res, command) =>
      res.continue(command.parse(args))
    }
}

sealed trait SubCommandParser[+A] {
  def parse(args: List[String]): CommandParseResult[(A, List[String])]
}




object ArgumentParser {

  def argument[A: ArgumentValueParser](name: String, shortName: String = "", description: String = ""): ArgumentParser[A] =
    SimpleValueParser(name, shortName, description, implicitly)

  def rest[A: ArgumentValueParser](name: String, description: String = ""): ArgumentParser[A] =
    RestValueParser(name, description, implicitly)

  def subcommands[A](commands: SubCommandParser[A]*): ArgumentParser[A] =
    SubCommandMultiParser(commands.toVector)

  def subcommand[B[_[_]]](name: String, description: String = "")(parser: B[ArgumentParser])(implicit cmdParser: CommandLineParser[B[ArgumentParser], B[Option], B[Id]]): SubCommandParser[B[Id]] =
    new SubCommandParser[B[Id]] {
      override def parse(args: List[String]): CommandParseResult[(B[Id], List[String])] = args match {
        case cmdName :: tail if cmdName === name =>
          parseIter(parser)(cmdParser.emptyValue)(args) match {
            case Some(value) => CommandParseResult.Value((value, Nil))
            case None => CommandParseResult.Error
          }

        case _ =>
          CommandParseResult.Skip
      }

      private def parseIter(parser: B[ArgumentParser])(acc: B[Option])(args: List[String])(implicit cmdParser: CommandLineParser[B[ArgumentParser], B[Option], B[Id]]): Option[B[Id]] =
        if(args.isEmpty)
          cmdParser.resolve(acc)
        else
          cmdParser.parse(parser)(cmdParser.emptyValue)(args) match {
            case CommandParseResult.Value((newAcc, tail)) => parseIter(parser)(newAcc)(tail)
            case CommandParseResult.Skip => None
            case CommandParseResult.Error => None
          }

    }

  implicit def stringArgumentValueParser: ArgumentValueParser[String] = new ArgumentValueParser[String] {
    override def parse(args: List[String]): Option[(String, List[String])] = args match {
      case value :: tail => Some((value, tail))
      case Nil => None
    }
  }

}
