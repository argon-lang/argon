package dev.argon.backend.jvm

import java.io.InputStream

import dev.argon.backend.jvm.jdkloader.{ClassPath, JarLibraryLoader, JarReader, JavaLibrary, JavaLibraryVisitor, JavaModule}
import dev.argon.compiler._
import dev.argon.compiler.core.{ArModule, Context, GlobalBinding, GlobalName, ModuleId, Namespace}
import dev.argon.compiler.loaders.{ModuleLoad, ModuleMetadata, ResourceIndicator, ResourceReader}
import zio._
import cats.implicits._
import zio.interop.catz.core._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import zio.blocking.Blocking
import java.lang.Runtime.{Version => JDKVersion}
import java.util.jar

import dev.argon.io.JarFileReader
import dev.argon.util.{MemoCacheStore, NamespacePath}

import scala.jdk.CollectionConverters._

object JavaLibraryLoader {

  def javaLibraryLoader[I <: ResourceIndicator, TContext <: JVMContext with Context.WithRes[JVMResourceIndicator[I]]](jkdVersion: JDKVersion, libraries: Seq[JavaModule], classPath: ClassPath)(blocking: Blocking.Service, res: ResourceReader.Service[I]): ModuleLoad.Service[JVMResourceIndicator[I], TContext] =
    new ModuleLoad.Service[JVMResourceIndicator[I], TContext] {
      override def loadResource(id: JVMResourceIndicator[I]): Managed[CompilationError, Option[ModuleMetadata[TContext]]] =
        id match {
          case JVMResourceIndicator.SystemLibrary(moduleName) =>
            Managed.succeed(
              libraries
                .find(_.getModuleName === moduleName)
                .map(javaModuleMetadata)
            )

          case JVMResourceIndicator.ClassPath() =>
            Managed.succeed(Some(javaClasspathMetadata(classPath)))

          case JVMResourceIndicator.Wrapped(inner) if inner.extension === "jar" =>
            res.getJarReader(inner, jkdVersion).mapM { jarReader =>
              ZIO.runtime[Any].flatMap { runtime =>
                val javaJarReader = new JavaJarReader(runtime, jarReader)

                blocking.effectBlocking {
                  JarLibraryLoader.loadJar(javaJarReader, jkdVersion)
                }
                  .catchAll(Compilation.unwrapThrowable)
                  .flatMap { javaLibrary =>
                    javaLibrary.visit(new JavaLibraryVisitor[IO[CompilationError, Option[ModuleMetadata[TContext]]]] {
                      override def visitModule(module: JavaModule): IO[CompilationError, Option[ModuleMetadata[TContext]]] =
                        IO.some(javaModuleMetadata(module))

                      override def visitClassPath(classPath: ClassPath): IO[CompilationError, Option[ModuleMetadata[TContext]]] =
                        Compilation.forErrors(JVMBackendErrors.JarNotModule(inner.show, DiagnosticSource.ReferencedModule(JavaLibrary.classpathModuleId)))
                    })
                  }
              }
            }

          case _ => Managed.succeed(None)
        }
    }

  private def javaModuleMetadata[TContext <: JVMContext](module: JavaModule): ModuleMetadata[TContext] =
    new ModuleMetadata[TContext] {
      override val descriptor: ModuleId = JavaLibrary.idFromModuleName(module.getModuleName)
      override val referencedModules: Vector[ModuleId] = module.getRequires.asScala.toVector.map(JavaLibrary.idFromModuleName)

      override def loadReference(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): Comp[ArModule[context.type, ReferencePayloadSpecifier]] = ???

    }

  private def javaClasspathMetadata[TContext <: JVMContext](classPath: ClassPath): ModuleMetadata[TContext] =
    new ModuleMetadata[TContext] {
      override val descriptor: ModuleId = JavaLibrary.classpathModuleId
      override val referencedModules: Vector[ModuleId] = Vector.empty

      override def loadReference(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): Comp[ArModule[context.type, ReferencePayloadSpecifier]] = ???
    }

  private class JavaLibraryMetadata[TContext <: JVMContext](val descriptor: ModuleId, val referencedModules: Vector[ModuleId], library: JavaLibrary, blocking: Blocking.Service) extends ModuleMetadata[TContext] {
    override def loadReference(ctx: TContext)(referencedModules2: Vector[ArModule[ctx.type, ReferencePayloadSpecifier]]): Comp[ArModule[ctx.type, ReferencePayloadSpecifier]] =
      for {
        classCache <- MemoCacheStore.make[CompilationError, (String, String), Option[GlobalBinding[ctx.type, ReferencePayloadSpecifier]]]
      } yield new ArModule[ctx.type, ReferencePayloadSpecifier] {
        override val context: ctx.type = ctx
        override val id: ModuleId = descriptor
        override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] = referencedModules2

        private def joinPackages(ns: NamespacePath): String =
          ns.ns.map(IdentifierEscape.escapeIdentifier).mkString(".")

        private def classToGlobalBinding(inputStream: InputStream): Comp[GlobalBinding[context.type, ReferencePayloadSpecifier]] = ???


        private def lookupClass(pkg: String, name: String): Comp[Option[GlobalBinding[context.type, ReferencePayloadSpecifier]]] =
          ZManaged.make(blocking.effectBlocking { Option(library.findClass(pkg, name)) })(isOpt => blocking.effectBlocking { isOpt.foreach { _.close() } }.orDie)
            .catchAll(Compilation.unwrapThrowableManaged)
            .use { inputStreamOpt =>
              ZIO.foreach(inputStreamOpt)(classToGlobalBinding)
            }


        override def lookupNamespaceValues[T](namespace: NamespacePath, name: GlobalName)(f: GlobalBinding[context.type, ReferencePayloadSpecifier] => Comp[Option[T]]): Comp[Vector[T]] =
          name match {
            case GlobalName.Normal(name) =>
              classCache
                .usingCreate((lookupClass _).tupled)
                .get((joinPackages(namespace), name))
                .flatMap { _.flatTraverse(f) }
                .map { _.toList.toVector }

            case _ => IO.succeed(Vector.empty)
          }

        override lazy val globalNamespace: Comp[Namespace[context.type, ReferencePayloadSpecifier]] = ???


      }
  }

  private class JavaJarReader(runtime: zio.Runtime[Any], jarReader: JarFileReader[Any, CompilationError]) extends JarReader {
    override def getJarName: String = runtime.unsafeRunTask(jarReader.jarName)

    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    override def getManifest: jar.Manifest =
      runtime.unsafeRunTask(jarReader.manifest.map(_.orNull))

    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    override def getEntryStream(path: String): InputStream =
      runtime.unsafeRunTask(
        jarReader.getEntryStream(path)
          .flatMap { streamOpt =>
            ZIO.foreach(streamOpt) { stream =>
              closeableInputStream(
                stream.toInputStream
              )
            }
          }
          .map { _.orNull }
      )

    private def closeableInputStream(managedInputStream: TaskManaged[InputStream]): Task[InputStream] =
      ZManaged.ReleaseMap.make.flatMap { releaseMap =>
        managedInputStream.zio.provide(((), releaseMap))
          .map { case (finalizer, inputStream) =>
            new InputStream {
              override def read(b: Array[Byte], off: Int, len: Int): Int = inputStream.read(b, off, len)
              override def read(): Int = inputStream.read()
              override def close(): Unit =
                try inputStream.close()
                finally try runtime.unsafeRunTask(finalizer(Exit.Success(())).unit)
                finally runtime.unsafeRunTask(releaseMap.releaseAll(Exit.Success(()), ExecutionStrategy.Sequential).unit)
            }
          }
          .onError { cause => releaseMap.releaseAll(Exit.Failure(cause), ExecutionStrategy.Sequential) }
      }
      .uninterruptible
  }

}
