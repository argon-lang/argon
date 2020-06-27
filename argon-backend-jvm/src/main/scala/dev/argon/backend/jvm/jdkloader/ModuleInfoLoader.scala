package dev.argon.backend.jvm.jdkloader

import java.io.InputStream
import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassReader, ClassVisitor, ModuleVisitor}
import zio.{IO, Task}
import dev.argon.backend.jvm._

import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.All"))
private[jdkloader] object ModuleInfoLoader {
  def getModuleInfo(moduleInfo: Path): Task[(String, Set[String])] = {
    val is = Files.newInputStream(moduleInfo)
    try getModuleInfo(is)
    finally is.close()
  }

  def getModuleInfo(inputStream: InputStream): Task[(String, Set[String])] = IO.effectSuspend {

    var moduleName: String = null
    val packages = mutable.HashSet[String]()

    val visitor = new ClassVisitor(asmVersion) {
      override def visitModule(name: String, access: Int, version: String): ModuleVisitor = {
        moduleName = name

        new ModuleVisitor(asmVersion) {

          override def visitExport(packaze: String, access: Int, modules: String*): Unit = {
            if(modules eq null) {
              packages += packaze
            }
          }
        }
      }
    }

    new ClassReader(inputStream).accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)

    if(moduleName eq null) IO.fail(new Exception("Could not load module info."))
    else IO.succeed((moduleName, packages.toSet))
  }
}
