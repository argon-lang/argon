package dev.argon.backend.jvm

import dev.argon.backend.ResourceAccess
import dev.argon.compiler.WrappedErrorListException
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import org.objectweb.asm.tree.MethodNode
import zio._
import zio.blocking.Blocking
import zio.stream.{Stream, ZStream}
import cats.implicits._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures", "org.wartremover.warts.Null", "org.wartremover.warts.Var", "dev.argon.warts.ZioEffect"))
object ExternMethodExtractor {

  private class MethodFindClassVisitor(foundMethods: ListBuffer[MethodNode]) extends ClassVisitor(asmVersion) {

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      val node = new MethodNode(asmVersion)
      foundMethods += node
      node
    }

  }

  private class SpecifierMethodVisitor extends MethodVisitor(asmVersion) {

    var specifier: Option[String] = None

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
      if(descriptor === "Argon/JVM/Annotations/Extern")
        new AnnotationVisitor(asmVersion) {
          override def visit(name: String, value: Any): Unit = {
            if(name === "specifier")
              value match {
                case value: String =>
                  specifier = Some(value)

                case _ => ()
              }
          }
        }
      else
        null

  }

  def getExterns[I <: ResourceIndicator: Tag](classFiles: Stream[Nothing, I]): RIO[Blocking with ResourceReader[I], Map[String, ResolvedExtern]] =
    classFiles
      .flatMap { classFile =>
        ZStream.unwrap(
          ZIO.access[ResourceReader[I]](_.get.readFile(classFile)).flatMap { classFileContent =>
            classFileContent.mapError(WrappedErrorListException.toThrowable).toInputStream.use { classFileIS =>
              ZIO.accessM[Blocking](_.get.effectBlocking {
                val reader = new ClassReader(classFileIS)
                val foundMethods = ListBuffer[MethodNode]()
                reader.accept(new MethodFindClassVisitor(foundMethods), ClassReader.SKIP_DEBUG)
                Stream.fromIterable(foundMethods.toList)
              })
            }
          }
        )
      }
      .mapM { method =>
        IO.effect {
          val specifierVisitor = new SpecifierMethodVisitor
          method.accept(specifierVisitor)
          specifierVisitor.specifier.map { (_, ResolvedExtern.Method(method)) }
        }
      }
      .collectSome
      .fold(Map.empty[String, ResolvedExtern]) { (map, methodPair) =>
        map |+| Map(methodPair)
      }

}
