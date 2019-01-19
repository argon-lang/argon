
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

val Unit_fileSpec = FileSpec(fileID = getFileId(), name = "Unit.argon")
val Unit_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Unit_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Unit"),
  AccessModifier.Public,
))
val Unit_classId = 0

val String_fileSpec = FileSpec(fileID = getFileId(), name = "String.argon")
val String_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  String_fileSpec,
  Ar_ns,
  GlobalName.NormalName("String"),
  AccessModifier.Public,
))
val String_classId = 1

val Int_fileSpec = FileSpec(fileID = getFileId(), name = "Int.argon")
val Int_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Int_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Int"),
  AccessModifier.Public,
))
val Int_classId = 2

val Bool_fileSpec = FileSpec(fileID = getFileId(), name = "Bool.argon")
val Bool_descriptor = ClassDescriptor.InNamespace(ClassDescriptorInNamespace(
  Bool_fileSpec,
  Ar_ns,
  GlobalName.NormalName("Bool"),
  AccessModifier.Public,
))
val Bool_classId = 3

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
  MethodOwnerDescriptor.ClassDescriptor(Int_descriptor)
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
    )),
  ),

  classes = Seq(
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
    )),

    // Ar.Bool
    Class.ClassDef(ClassDefinition(
      descriptor = Bool_descriptor,
      isOpen = false,
      isSealed = false,
      isAbstract = false,
      signature = ClassSignature(
        parameters = Seq(),
        baseClass = None,
        baseTraits = Seq(TraitType(Object_traitId, Seq())),
      ),
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



