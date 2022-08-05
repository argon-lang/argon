package dev.argon.plugin.test

import java.nio.file.{Files, Path}
import scala.quoted.*

object ReadFileCompileTime {

  inline def readDirectory(inline path: String, inline keepPath: (Boolean, String) => Boolean): CompileTimeFileSystem =
    ${ readDirectoryImpl('path, 'keepPath) }

  inline def readDirectory(inline path: String): CompileTimeFileSystem =
    readDirectory(path, (_, _) => true)


  private def readDirectoryImpl(dirExpr: Expr[String], keepPathExpr: Expr[(Boolean, String) => Boolean])(using quotes: Quotes): Expr[CompileTimeFileSystem] =
    import scala.jdk.CollectionConverters.*

    def keepPath(isDir: Boolean, path: String): Boolean =
      import quotes.reflect.*
      def evalKnownFunctions(t: Term): Term =
        t match {
          case Inlined(_, _, expansion) => evalKnownFunctions(expansion)

          case Apply(sel @ Select(a, "||"), List(b)) =>
            (evalKnownFunctions(a), evalKnownFunctions(b)) match {
              case (Literal(BooleanConstant(a)), Literal(BooleanConstant(b))) => Literal(BooleanConstant(a || b))
              case (a, b) => Apply(Select.copy(sel)(a, "||"), List(b))
            }

          case Apply(sel @ Select(a, "endsWith"), List(b)) =>
            (evalKnownFunctions(a), evalKnownFunctions(b)) match {
              case (Literal(StringConstant(a)), Literal(StringConstant(b))) => Literal(BooleanConstant(a.endsWith(b)))
              case (a, b) => Apply(Select.copy(sel)(a, "endsWith"), List(b))
            }

          case _ => t
        }

      val e1 = Expr.betaReduce('{ $keepPathExpr(${ Expr(isDir) }, ${ Expr(path) }) })
      val e2 = evalKnownFunctions(e1.asTerm).asExpr.asInstanceOf[Expr[Boolean]]
      Expr.betaReduce(e2).valueOrAbort

    def impl(path: Path): Expr[CompileTimeFileSystem] =
      if Files.isDirectory(path) then
        val dirEntries = Expr.ofSeq(
          Files.list(path)
            .iterator()
            .asScala
            .filter { subPath =>
              val isDir = Files.isDirectory(subPath)
              val name = subPath.getFileName.toString
              keepPath(isDir, name)
            }
            .map { subPath =>
              val subPathName = Expr(path.relativize(subPath).getFileName.toString)
              val subfs = impl(subPath)
              '{ $subPathName -> $subfs }
            }
            .toSeq
        )

        '{ CompileTimeFileSystem.Directory($dirEntries.toMap) }
      else
        val data = Expr(Files.readString(path))
        '{ CompileTimeFileSystem.File($data) }
      end if

    impl(Path.of(dirExpr.valueOrAbort))
  end readDirectoryImpl


}
