package dev.argon

import shapeless._

sealed trait CommandLineParser[Parser, Partial, Res] {
  def emptyValue: Partial
  def parse(parser: Parser)(acc: Partial)(args: List[String]): CommandParseResult[(Partial, List[String])]
  def resolve(value: Partial): Option[Res]
}

object CommandLineParser {

  def parse[F[_[_]]](parser: F[ArgumentParser])(args: List[String])(implicit cmdParser: CommandLineParser[F[ArgumentParser], F[Option], F[Id]]): Option[F[Id]] =
    parseIter(parser)(cmdParser.emptyValue)(args)

  private def parseIter[F[_[_]]](parser: F[ArgumentParser])(acc: F[Option])(args: List[String])(implicit cmdParser: CommandLineParser[F[ArgumentParser], F[Option], F[Id]]): Option[F[Id]] =
    if(args.isEmpty)
      cmdParser.resolve(acc)
    else
      cmdParser.parse(parser)(cmdParser.emptyValue)(args) match {
        case CommandParseResult.Value((newAcc, tail)) => parseIter(parser)(newAcc)(tail)
        case CommandParseResult.Skip => None
        case CommandParseResult.Error => None
      }

  object Implicits {

    implicit def parserNil: CommandLineParser[HNil, HNil, HNil] = new CommandLineParser[HNil, HNil, HNil] {
      override def emptyValue: HNil = HNil
      override def parse(parser: HNil)(acc: HNil)(args: List[String]): CommandParseResult[(HNil, List[String])] = CommandParseResult.Skip
      override def resolve(value: HNil): Option[HNil] = Some(HNil)
    }

    implicit def parserCons[H, TParser <: HList, TPartial <: HList, TRes <: HList]
    (implicit
     tailParser: CommandLineParser[TParser, TPartial, TRes]
    ): CommandLineParser[ArgumentParser[H] :: TParser, Option[H] :: TPartial, H :: TRes] = new CommandLineParser[ArgumentParser[H] :: TParser, Option[H] :: TPartial, H :: TRes] {

      override def emptyValue: Option[H] :: TPartial = None :: tailParser.emptyValue
      override def parse(parser: ArgumentParser[H] :: TParser)(acc: Option[H] :: TPartial)(args: List[String]): CommandParseResult[(Option[H] :: TPartial, List[String])] =
        parser.head.parse(args) match {
          case CommandParseResult.Value((value, tail)) =>
            (acc.head match {
              case Some(oldValue) => parser.head.combine(oldValue, value)
              case None => Some(value)
            }) match {
              case Some(value) => CommandParseResult.Value((Some(value) :: acc.tail, tail))
              case None => CommandParseResult.Error
            }

          case CommandParseResult.Skip => tailParser.parse(parser.tail)(acc.tail)(args).map {
            case (tailPartial, tailArgs) =>
              (acc.head :: tailPartial, tailArgs)
          }
          case CommandParseResult.Error => CommandParseResult.Error
        }

      override def resolve(value: Option[H] :: TPartial): Option[H :: TRes] = for {
        head <- value.head
        tail <- tailParser.resolve(value.tail)
      } yield head :: tail
    }

    implicit def parserGen[A[_[_]], Parser <: HList, Partial <: HList, Res <: HList]
    (implicit
      genParser: Generic.Aux[A[ArgumentParser], Parser],
      genPartial: Generic.Aux[A[Option], Partial],
      genRes: Generic.Aux[A[Id], Res],
      listParser: CommandLineParser[Parser, Partial, Res],
    ): CommandLineParser[A[ArgumentParser], A[Option], A[Id]] = new CommandLineParser[A[ArgumentParser], A[Option], A[Id]] {
      override def emptyValue: A[Option] = genPartial.from(listParser.emptyValue)

      override def parse(parser: A[ArgumentParser])(acc: A[Option])(args: List[String]): CommandParseResult[(A[Option], List[String])] =
        listParser.parse(genParser.to(parser))(genPartial.to(acc))(args).map {
          case (partial, tail) =>
            (genPartial.from(partial), tail)
        }

      override def resolve(value: A[Option]): Option[A[Id]] =
        listParser.resolve(genPartial.to(value)).map(genRes.from)
    }

  }

}
