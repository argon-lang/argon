
import java.io.File

import com.mi3software.argon.module._
import com.mi3software.argon.librarygen._

val Ar_ns = Namespace(Seq("Ar"))

var nextFileId = 0
def getFileId(): Int = {
  val res = nextFileId
  nextFileId += 1
  res
}


val Object_fileSpec = FileSpec(fileID = getFileId(), name = "Object.argon")
val Object_descriptor = TraitDescriptor.InNamespace(TraitDescriptorInNamespace(
  Object_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Object"),
  AccessModifier.Public,
))
val Object_traitId = 0
val Object_metaClassId = 0

val Type_fileSpec = FileSpec(fileID = getFileId(), name = "Type.argon")
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

val Unit_fileSpec = FileSpec(fileID = getFileId(), name = "Unit.argon")
val Unit_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Unit_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Unit"),
  AccessModifier.Public,
))
val Unit_classId = 8
val Unit_metaClassId = 9

val String_fileSpec = FileSpec(fileID = getFileId(), name = "String.argon")
val String_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  String_fileSpec,
  Ar_ns,
  GlobalName.NormalName("String"),
  AccessModifier.Public,
))
val String_classId = 10
val String_metaClassId = 11

val Int_fileSpec = FileSpec(fileID = getFileId(), name = "Int.argon")
val Int_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Int_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Int"),
  AccessModifier.Public,
))
val Int_classId = 12
val Int_metaClassId = 13

val Puts_fileSpec = FileSpec(fileID = getFileId(), name = "Puts.argon")
val Puts_descriptor = FunctionDescriptor.InNamespace(FunctionDescriptorInNamespace(
  Puts_fileSpec,
  Ar_ns,
  GlobalName.NormalName("puts"),
  AccessModifier.Public,
))

val Int_to_s_descriptor = MethodDescriptor(
  MemberName.Name("to_s"),
  AccessModifier.Public,
  ClassLikeDescriptor.ClassDescriptor(Int_descriptor)
)


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
        baseTraits = Seq(),
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
        baseTraits = Seq(),
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
        baseTraits = Seq(),
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

    // Ar.Unit
    Class.ClassDef(ClassDefinition(
      descriptor = Unit_descriptor,
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = None,
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(Unit_metaClassId)
    )),

    // Ar.Unit.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(Unit_descriptor)),
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

    // Ar.String
    Class.ClassDef(ClassDefinition(
      descriptor = String_descriptor,
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = None,
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(String_metaClassId)
    )),

    // Ar.String.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(String_descriptor)),
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

    // Ar.Int
    Class.ClassDef(ClassDefinition(
      descriptor = Int_descriptor,
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = None,
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
      metaClassSpecifier = MetaClassSpecifier(String_metaClassId)
    )),

    // Ar.Int.<MetaClass>
    Class.ClassDef(ClassDefinition(
      descriptor = ClassDescriptor.MetaClass(ClassDescriptorMetaClass(Int_descriptor)),
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

  functions = Seq(
    Function.FuncDef(FunctionDefinition(
      descriptor = Puts_descriptor,
      effects = EffectInfo(isPure = false),
      signature = FunctionSignature(
        parameters = Seq(
          Parameter(Seq(
            ParameterElement(
              paramType = Type.ClassType(ClassType(
                classId = String_classId,
                typeArguments = Seq(),
              )),
              name = "str"
            )
          )),
        ),
        returnType = Type.ClassType(ClassType(
          classId = Unit_classId,
          typeArguments = Seq(),
        ))
      ),

    ))
  ),

  methods = Seq(
    Method.MethodDef(MethodDefinition(
      descriptor = Int_to_s_descriptor,
      effects = EffectInfo(isPure = false),
      isVirtual = false,
      isAbstract = false,
      isImplicitOverride = false,
      isFinal = false,
      signature = MethodSignature(
        parameters = Seq(
          Parameter(Seq()),
        ),
        returnType = Type.ClassType(ClassType(
          classId = String_classId,
          typeArguments = Seq(),
        ))
      ),
      implementation = None,
    ))
  ),
  classConstructors = Seq(),
)


@main
def main(scriptPath: String): Unit = {
  writeModule(new File(new File(scriptPath).getParentFile, "Argon.Core.armodule").getPath, module)
}



