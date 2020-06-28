package dev.argon.warts

import org.wartremover.{WartTraverser, WartUniverse}
import scala.reflect.NameTransformer
import zio._
import zio.stream._

object ZioEffect extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    val bannedSymbols =
      for {
        name <- Seq(
          "apply",

          "effect", "effectTotal",
          "effectSuspend", "effectSuspendTotal", "effectSuspendTotalWith", "effectSuspendWith",
          "effectAsync", "effectAsyncInterrupt", "effectAsyncM", "effectAsyncMaybe",

          "fromJavaIteratorTotal", "fromIterator", "fromJavaIterator",
          "fromIteratorEffect", "fromJavaIteratorEffect",
          "fromIteratorManaged", "fromJavaIteratorManaged",

          "fromFile",
        )

        termName = TermName(NameTransformer.encode(name))

        t <- Seq(
          typeOf[ZIO.type], typeOf[IO.type], typeOf[RIO.type], typeOf[UIO.type], typeOf[URIO.type], typeOf[Task.type],
        ) ++ (
          if(name != "apply")
            Seq(typeOf[ZManaged.type], typeOf[Managed.type], typeOf[Stream.type], typeOf[ZStream.type])
          else
            Seq.empty
        )

        symbol = t.member(termName) if symbol != NoSymbol

      } yield t.member(termName)

    new u.Traverser {
      override def traverse(tree: u.universe.Tree): Unit =
        tree match {
          case t if hasWartAnnotation(u)(t) =>

          case rt: RefTree if bannedSymbols.contains(rt.symbol) =>
            error(u)(tree.pos, "ZIO effects are banned")

          case _ =>
            super.traverse(tree)
        }
    }
  }
}
