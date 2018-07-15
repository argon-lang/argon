
import java.io.File

import com.mi3software.argon.module._
import com.mi3software.argon.librarygen._

val Ar_ns = Namespace(Seq("Ar"))

val Object_fileSpec = FileSpec(fileID = 0, name = "Object.argon")
val Object_descriptor = TraitDescriptor.InNamespace(TraitDescriptorInNamespace(
  Object_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Object"),
  AccessModifier.Public,
))
val Object_traitId = 0
val Object_metaClassId = 0

val Type_fileSpec = FileSpec(fileID = 0, name = "Type.argon")
val Type_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Type_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Type"),
  AccessModifier.Public,
))
val Type_classId = 1
val Type_metaClassId = 2

val MetaClass_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Type_fileSpec,
  Ar_ns,
  GlobalName.NormalName("MetaClass"),
  AccessModifier.Public,
))
val MetaClass_classId = 3

val Trait_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Type_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Trait"),
  AccessModifier.Public,
))
val Trait_classId = 4
val Trait_metaClassId = 5

val Class_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Type_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Class"),
  AccessModifier.Public,
))
val Class_classId = 6
val Class_metaClassId = 7


val module = Module(
  formatVersion = moduleFormatVersion,
  name = "Argon.Core",
  isInterfaceOnly = true,

  traits = Seq(
    // Ar.Object
    Trait.TraitDef(TraitDefinition(
      descriptor = Object_descriptor,

      isSealed = false,

      signature = TraitSignature(
        parameters = Seq(),
        baseTraits = Seq(),
      ),

      metaClassSpecifier = MetaClassSpecifier(Object_metaClassId)
    )),
  ),

  classes = Seq(
    // Ar.Object.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.TraitMetaClass(ClassDescriptorTraitMetaClass(Object_descriptor)),
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Trait_classId, Seq())),
        baseTraits = Seq(),
      ),
      metaClassSpecifier = MetaClassSpecifier(MetaClass_classId)
    )),

    // Ar.Type
    Class.ClassDef(ClassDefinition(
      descriptor = Type_descriptor,
      isOpen = true,
      isSealed = true,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = None,
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(Type_metaClassId)
    )),

    // Ar.Type.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(Type_descriptor)),
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Class_classId, Seq())),
        baseTraits = Seq(),
      ),
      metaClassSpecifier = MetaClassSpecifier(MetaClass_classId)
    )),

    // Ar.MetaClass
    Class.ClassDef(ClassDefinition(
      descriptor = MetaClass_descriptor,
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Class_classId, Seq())),
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(MetaClass_classId)
    )),

    // Ar.Trait
    Class.ClassDef(ClassDefinition(
      descriptor = Trait_descriptor,
      isOpen = true,
      isSealed = true,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Type_classId, Seq())),
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(Trait_metaClassId)
    )),

    // Ar.Trait.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(Trait_descriptor)),
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Class_classId, Seq())),
        baseTraits = Seq(),
      ),
      metaClassSpecifier = MetaClassSpecifier(MetaClass_classId)
    )),

    // Ar.Class
    Class.ClassDef(ClassDefinition(
      descriptor = Class_descriptor,
      isOpen = true,
      isSealed = true,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Type_classId, Seq())),
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(Class_metaClassId)
    )),

    // Ar.Class.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(Class_descriptor)),
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = Some(ClassType(Class_classId, Seq())),
        baseTraits = Seq(),
      ),
      metaClassSpecifier = MetaClassSpecifier(MetaClass_classId)
    )),

  ),

  dataConstructors = Seq(),
  functions = Seq(),
  methods = Seq(),
  classConstructors = Seq(),
)


@main
def main(scriptPath: String): Unit = {
  writeModule(new File(new File(scriptPath).getParentFile, "Argon.Core.armodule").getPath, module)
}



