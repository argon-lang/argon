package dev.argon.plugins.tube

import dev.argon.io.*
import dev.argon.plugin.tube.SerializedTube
import dev.argon.tube as t
import dev.argon.tube.GeneratedEnumInstances.given
import dev.argon.compiler.module.ModulePath
import dev.argon.util.{*, given}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.*
import zio.stream.ZStream

import java.io.IOException

trait SerializedTubeResource[R, E >: IOException] extends ZipStreamResource[R, E] with ZipStreamResource.Impl[R, E] {
  def asSerializedTube: ZIO[R & Scope, E, SerializedTube[R, E]]

  override def asZip: ZIO[R & Scope, E, ZipStreamResource.Zip[R, E]] =
    asSerializedTube.map { tube =>
      new ZipStreamResource.Zip[R, E] {

        private def entryWithBinaryResource(res: BinaryResource[R, E], name: String): ZipStreamResource.Entry[R, E] =
          new ZipStreamResource.Entry[R, E] {
            override val path: String = name

            override def value: BinaryResource[R, E] = res
          }

        private def entryWithDirectoryResource(res: DirectoryResource[R, E, BinaryResource], name: String): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          res.contents.flatMap {
            case DirectoryEntry.Subdirectory(subName, resource) => entryWithDirectoryResource(resource, name + "/" + subName)
            case DirectoryEntry.File(subName, resource) => ZStream(entryWithBinaryResource(resource, name + "/" + subName))
          }

        private def entryWithResource(res: FileSystemResource[R, E], name: String): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          res match {
            case res: BinaryResource[R, E] => ZStream(entryWithBinaryResource(res, name))
            case res: DirectoryResource[R, E, BinaryResource] => entryWithDirectoryResource(res, name)
          }

        private def entryWithName[A <: GeneratedMessage: GeneratedMessageCompanion](a: ZIO[R, E, A], name: String): ZipStreamResource.Entry[R, E] =
          val res = new ProtobufResource.Impl[R, E, A] with Resource.WithoutFileName {
            override def asMessage: ZIO[R, E, A] = a
          }
          entryWithBinaryResource(res, name)
        end entryWithName

        private def entry[A <: GeneratedMessage: GeneratedMessageCompanion](a: ZIO[R, E, A], args: Any*): ZipStreamResource.Entry[R, E] =
          entryWithName(a, Paths.get[A](args*))

        private def entry[A <: GeneratedMessage : GeneratedMessageCompanion](a: A, args: Any*): ZipStreamResource.Entry[R, E] =
          entryWithName(ZIO.succeed(a), Paths.get[A](args*))

        override def entries: ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream.unwrap(
            tube.metadata.map { metadata =>
              ZStream(
                entry(tube.version),
                entry(metadata),
              ) ++
                resourceEntries(metadata) ++
                moduleEntries(metadata)
            }
          )

        private def resourceEntries(metadata: t.Metadata): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream.fromIterable(metadata.optionResources).flatMap { id =>
            ZStream.unwrap(tube.getResource(id).map { res => entryWithResource(res, Paths.get[t.Resource](id)) })
          }

        private def moduleEntries(metadata: t.Metadata): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream.fromIterable(metadata.modules).flatMap { modulePath =>
            ZStream.unwrap(
              tube.getModule(modulePath).map { module =>
                val arModulePath = ModulePath(modulePath.name)
                ZStream(entry(module, arModulePath.urlEncode)) ++
                  ZStream.fromIterable(module.elements).flatMap { nameGroup =>
                    ZStream.fromIterable(nameGroup.declaredElements).flatMap { elem =>
                      elem.`type` match {
                        case t.ModuleDefinition.ElementDeclaration.Type.Class =>
                          ZStream.unwrap(tube.getClass(elem.id).map(classEntries(elem.id)))

                        case t.ModuleDefinition.ElementDeclaration.Type.Trait =>
                          ZStream.unwrap(tube.getTrait(elem.id).map(traitEntries(elem.id)))

                        case t.ModuleDefinition.ElementDeclaration.Type.Function =>
                          ZStream.unwrap(tube.getFunction(elem.id).map(functionEntries(elem.id)))

                        case t.ModuleDefinition.ElementDeclaration.Type.Unknown | t.ModuleDefinition.ElementDeclaration.Type.Unrecognized(_) =>
                          ZStream.empty
                      }
                    }
                  }
              }
            )
          }

        private def classEntries(id: BigInt)(classDef: t.ClassDefinition): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream(entry(classDef, id)) ++
            methodGroupEntries(classDef.methods) ++
            methodGroupEntries(classDef.staticMethods) ++
            ZStream.fromIterable(classDef.constructors).flatMap { ctorMember =>
              ZStream.unwrap(tube.getClassConstructor(ctorMember.id).map(classConstructorEntries(ctorMember.id)))
            }

        private def traitEntries(id: BigInt)(traitDef: t.TraitDefinition): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream(entry(traitDef, id)) ++
            methodGroupEntries(traitDef.methods) ++
            methodGroupEntries(traitDef.staticMethods)

        private def functionEntries(id: BigInt)(funcDef: t.FunctionDefinition): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream(entry(funcDef, id))

        private def methodGroupEntries(methods: Seq[t.MethodMemberGroup]): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream.fromIterable(methods).flatMap { methodGroup =>
            ZStream.fromIterable(methodGroup.methods).flatMap { methodMember =>
              ZStream.unwrap(tube.getMethod(methodMember.id).map(methodEntries(methodMember.id)))
            }
          }

        private def methodEntries(id: BigInt)(methodDef: t.MethodDefinition): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream(entry(methodDef, id))

        private def classConstructorEntries(id: BigInt)(classCtorDef: t.ClassConstructorDefinition): ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream(entry(classCtorDef, id))

      }
    }
}
