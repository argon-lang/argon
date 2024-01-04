package dev.argon.esexpr.generator

import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.schema.*
import dev.argon.util.{*, given}
import org.apache.commons.text.StringEscapeUtils
import zio.{Scope, Task, ZIO}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[generator] final class JavaRunJSGenerator(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String,
  javaApiPackageName: String,
  jsUtilPackageName: String,
) extends GeneratorBase with JavaGeneratorNaming with JavaGeneratorUtil {

  private val jsValueName = "org.graalvm.polyglot.Value"

  private var nextValueNum: Int = 0

  private def getNextValueName(): String =
    val name = s"value$nextValueNum"
    nextValueNum += 1
    name
  end getNextValueName


  override def writePackageQualifier(): Unit =
    w(javaApiPackageName)
    w(".")
  end writePackageQualifier

  def generateWrapCodec(definition: Definition): Unit =
    w("package ")
    w(packageName)
    wl(";")

    val definitionName = GeneratorUtil.getDefinitionName(definition)
    val definitionTypeParams = GeneratorUtil.getDefinitionTypeParameters(definition)
    val isRef = GeneratorUtil.isReferenceType(definition)

    writeClass(definitionName + "-wrap-codec", `final` = true) {

      definition match {
        case definition: Definition.TypeEnum =>
          w("public static abstract class Codec<T extends ")
          w(javaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          wl("> {")
          indent {
            w("public abstract <")
            writeCommaListSingleLine[String](value => {
              w("T")
              writeTypeName(value)
            })(definition.values)
            w("> ")
            w(jsUtilPackageName)
            w(".WrapCodec<")
            w(javaApiPackageName)
            w(".")
            writeTypeName(definitionName)
            w(".Match<T")
            for value <- definition.values do
              w(", T")
              writeTypeName(value)
            end for
            w(">> matchCodec(")
            writeCommaListSingleLine[String](value => {
              w(jsUtilPackageName)
              w(".WrapCodec<")
              w("T")
              writeTypeName(value)
              w("> ")

              writeValueName(value)
              w("Codec")
            })(definition.values)
            wl(");")
          }
          wl("}")

        case definition: Definition.TypeStruct =>
          w("public interface Codec<TS extends ")
          w(javaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          wl("<TS>> {")
          indent {
            for member <- definition.members do
              w(jsUtilPackageName)
              w(".WrapCodec<")
              w(javaApiPackageName)
              w(".")
              writeTypeName(definitionName)
              w(".")
              writeTypeName(member)
              w("<TS>> ")
              writeValueName(member + "-codec")
              wl("();")
            end for
          }
          wl("}")


        case _ =>
          def writeCodecBody(): Unit =
            wl("@Override")
            w(s"public $jsValueName wrap(")
            w(javaApiPackageName)
            w(".")
            writeTypeName(definitionName)
            if isRef then writeTypeArgs(definitionTypeParams)
            wl(s" $reservedValueName) {")
            indent {
              wl("synchronized(env.lock) {")
              indent {
                generateWrapImpl(definition)
              }
              wl("}")
            }
            wl("}")
            wl("@Override")
            w(s"public ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(definitionName)
            if isRef then writeTypeArgs(definitionTypeParams)
            w(" unwrap(")
            w(jsValueName)
            wl(s" $reservedValueName) {")
            indent {
              wl("synchronized(env.lock) {")
              indent {
                generateUnwrapImpl(definition)
              }
              wl("}")
            }
            wl("}")
          end writeCodecBody


          w("public static <E extends java.lang.Throwable")
          for param <- definitionTypeParams do
            w(", ")
            writeTypeParameter(param)
          end for
          w("> ")
          w(jsUtilPackageName)
          w(".WrapCodec<")
          w(javaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          if isRef then writeTypeArgs(definitionTypeParams)
          w("> codec(")
          w(jsUtilPackageName)
          w(".JSEnv<E> env")
          for param <- definitionTypeParams do
            w(", ")
            param.`enum` match {
              case Some(enumName) =>
                w(packageName)
                w(".")
                writeTypeName(enumName)
                w("WrapCodec.Codec<")
                writeTypeName(param.name)
                w(">")


              case None =>
                param.tuple match {
                  case Some(tupleName) =>
                    w(packageName)
                    w(".")
                    writeTypeName(tupleName)
                    w("WrapCodec.Codec<")
                    writeTypeName(param.name)
                    w(">")

                  case None =>
                    w(jsUtilPackageName)
                    w(".WrapCodec<")
                    writeTypeName(param.name)
                    w(">")
                }
            }
            w(" ")
            writeValueName(param.name + "-wrap-codec")
          end for
          wl(") {")
          indent {
            w("return new ")
            w(jsUtilPackageName)
            w(".WrapCodec<")
            w(javaApiPackageName)
            w(".")
            writeTypeName(definitionName)
            if isRef then writeTypeArgs(definitionTypeParams)
            wl(">() {")
            indent {
              writeCodecBody()
            }
            wl("};")
          }
          wl("}")
      }



    }
  end generateWrapCodec

  def generateWrapImpl(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(ctor) =>
        wl(s"java.util.Map<String, Object> map = new java.util.HashMap<>();")
        ctor.parameters.foreach(generateWrapImplConstructorParameter(reservedValueName))
        wl("return env.context.asValue(org.graalvm.polyglot.proxy.ProxyObject.fromMap(map));")


      case Definition.Enum(name, constructors*) =>
        wl(s"return switch($reservedValueName) {")
        indent {
          for ctor <- constructors do
            w("case ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(ctor.name)
            w(" ")

            val valueName = getNextValueName()
            w(valueName)
            wl(" -> {")
            indent {
              wl(s"java.util.Map<String, Object> map = new java.util.HashMap<>();")
              w("map.put(\"type\", env.context.asValue(\"")
              w(StringEscapeUtils.escapeJava(ctor.name).nn)
              wl("\"));")
              ctor.parameters.foreach(generateWrapImplConstructorParameter(valueName))
              wl("yield env.context.asValue(org.graalvm.polyglot.proxy.ProxyObject.fromMap(map));")
            }
            wl("}")
          end for
        }
        wl("};")

      case Definition.Const(_, _, _) => throw new UnsupportedOperationException()

      case Definition.SimpleEnum(name, values*) =>
        wl(s"return switch($reservedValueName) {")
        indent {
          for value <- values do
            w("case ")
            w(getEnumCaseName(value))
            w(" -> env.context.asValue(\"")
            w(StringEscapeUtils.escapeJava(value).nn)
            wl("\");")
          end for
        }
        wl("};")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        wl(s"return switch($reservedValueName) {")
        indent {
          for enumCase <- cases do
            w("case ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(enumCase.name)
            writeTypeArgs(typeParameters)
            w(" ")

            val caseValueName = getNextValueName()
            w(caseValueName)
            wl(" -> {")
            indent {
              w(jsValueName)
              wl(""" obj = env.context.eval("js", "({})");""")
              w("obj.putMember(\"type\", \"")
              w(StringEscapeUtils.escapeJava(enumCase.name).nn)
              wl("\");")
              enumCase.members.foreach(generateWrapImplInterfaceMember(caseValueName))
              wl("yield obj;")
            }
            wl("}")
          end for
        }
        wl("};")

      case interfaceType: Definition.Interface =>
        w(jsValueName)
        wl(""" obj = env.context.eval("js", "({})");""")

        val allMembers = getAllMembers(interfaceType)

        allMembers.values.foreach(generateWrapImplInterfaceMember(reservedValueName))
        wl("return obj;")

      case Definition.TypeEnum(name, values*) => throw new UnsupportedOperationException()
      case Definition.TypeStruct(name, values*) => throw new UnsupportedOperationException()

      case _: Definition.Extern => throw new UnsupportedOperationException()
    }

  def generateWrapImplConstructorParameter(valueName: String)(param: Parameter): Unit =
    w("map.put(\"")
    w(StringEscapeUtils.escapeJava(TypeScriptGeneratorNaming.determineValueName(param.name)).nn)
    w("\", ")

    writeParameterCodec(param)
    w(".wrap(")
    w(valueName)
    w(".")
    writeValueName(param.name)
    wl("()));")
  end generateWrapImplConstructorParameter

  def generateWrapImplInterfaceMember(valueName: String)(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        w("""env.context.eval("js", "(o, name, f) => Object.defineProperty(o, name, { get: f })").execute(obj, """)
        w("\"")
        w(StringEscapeUtils.escapeJava(TypeScriptGeneratorNaming.determineValueName(name)).nn)
        w("\", (")
        w(jsUtilPackageName)
        w(".JSFunction0)(() -> ")
        writeWrapCodecOfType(fieldType)
        w(".wrap(")
        w(valueName)
        w(".")
        writeValueName(name)
        wl("())));")

      case Member.Method(name, _, async, params, returnType) =>
        w("""env.context.eval("js", "(o, name, f) => { o[name] = f; }").execute(obj, """)
        w("\"")
        w(StringEscapeUtils.escapeJava(TypeScriptGeneratorNaming.determineValueName(name)).nn)
        w("\", (")
        w(jsUtilPackageName)
        w(s".JSFunction${params.size})((")
        writeCommaListSingleLine[MethodParameter] {
          case MethodParameter.PositionalParam(name, _) =>
            writeValueName(name)
        }(params)
        w(") -> ")
        if async then w("env.toJSPromise(() -> ")
        writeWrapCodecOfType(returnType)
        w(".wrap(")
        w(valueName)
        w(".")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine[MethodParameter] {
          case MethodParameter.PositionalParam(name, paramType) =>
            writeWrapCodecOfType(paramType)
            w(".unwrap(")
            writeValueName(name)
            w(")")
        }(params)
        w("))")
        if async then w(")")
        wl("));")
    }


  def generateUnwrapImpl(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(constructor) =>
        w("return new ")
        w(javaApiPackageName)
        w(".")
        writeTypeName(constructor.name)
        wl("(")
        writeCommaListMultiline[Parameter](param => {
          writeParameterCodec(param)
          w(s".unwrap($reservedValueName.getMember(\"")
          w(StringEscapeUtils.escapeJava(param.name).nn)
          w("\"))")
        })(constructor.parameters)
        wl(");")

      case Definition.Enum(name, cases*) =>
        wl(s"return switch($reservedValueName.getMember(\"type\").asString()) {")
        indent {
          for enumCase <- cases do
            w("case \"")
            w(StringEscapeUtils.escapeJava(enumCase.name).nn)
            wl("\" ->")
            indent {
              w("new ")
              w(javaApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(enumCase.name)
              wl("(")
              indent {
                writeCommaListMultiline[Parameter](param => {
                  writeParameterCodec(param)
                  w(s".unwrap($reservedValueName.getMember(\"")
                  w(StringEscapeUtils.escapeJava(param.name).nn)
                  w("\"))")
                })(enumCase.parameters)
              }
              wl(");")
            }
          end for
          wl("default -> throw new RuntimeException(\"Unexpected JS value\");")
        }
        wl("};")

      case Definition.Const(name, dataType, value) => throw new UnsupportedOperationException()
      case Definition.SimpleEnum(name, values*) =>
        wl(s"return switch($reservedValueName.asString()) {")
        indent {
          for value <- values do
            w("case \"")
            w(StringEscapeUtils.escapeJava(value).nn)
            w("\" -> ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            w(getEnumCaseName(value))
            wl(";")
          end for
          wl("default -> throw new RuntimeException(\"Unexpected JS value\");")
        }
        wl("};")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        wl(s"return switch($reservedValueName.getMember(\"type\").asString()) {")
        indent {
          for enumCase <- cases do
            w("case \"")
            w(StringEscapeUtils.escapeJava(enumCase.name).nn)
            wl("\" -> {")
            indent {
              w("yield new ")
              w(javaApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(enumCase.name)
              writeTypeArgs(typeParameters)
              wl("() {")
              indent {
                enumCase.members.foreach(generateUnwrapImplInterfaceMember)
              }
              wl("};")
            }
            wl("}")
          end for
          wl("default -> throw new RuntimeException(\"Unexpected JS value\");")
        }
        wl("};")

      case interfaceType @ Definition.Interface(name, typeParameters, _, members*) =>
        w("return new ")
        w(javaApiPackageName)
        w(".")
        writeTypeName(name)
        writeTypeArgs(typeParameters)
        wl("() {")
        indent {
          val allMembers = getAllMembers(interfaceType)
          allMembers.values.foreach(generateUnwrapImplInterfaceMember)
        }
        wl("};")

      case Definition.TypeEnum(name, values*) => throw new UnsupportedOperationException()
      case Definition.TypeStruct(name, members*) => throw new UnsupportedOperationException()
      case _: Definition.Extern => throw new UnsupportedOperationException()
    }


  def writeTypeArgs(typeParameters: Seq[TypeParameter]): Unit =
    w("<E")
    for param <- typeParameters do
      w(", ")
      writeTypeName(param.name)
    end for
    w(">")

  end writeTypeArgs

  def generateUnwrapImplInterfaceMember(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        wl("@Override")
        w("public ")
        writeType(fieldType)
        w(" ")
        writeValueName(name)
        wl("() {")
        indent {
          wl("synchronized(env.lock) {")
          indent {
            w("return ")
            writeWrapCodecOfType(fieldType)
            w(s".unwrap($reservedValueName.getMember(\"")
            w(StringEscapeUtils.escapeJava(TypeScriptGeneratorNaming.determineValueName(name)).nn)
            wl("\"));")
          }
          wl("}")
        }
        wl("}")

      case Member.Method(name, _, async, params, returnType) =>
        wl("@Override")
        w("public ")
        writeType(returnType)
        w(" ")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine(writeMethodParameter)(params)
        w(")")
        if async then w(" throws E, java.io.IOException, java.lang.InterruptedException")
        wl("")
        wl(" {")
        indent {
          wl(s"org.graalvm.polyglot.Value esexpr_reserved_result;")
          wl("synchronized(env.lock) {")
          indent {
            w(s"esexpr_reserved_result = $reservedValueName.invokeMember(\"")
            w(StringEscapeUtils.escapeJava(TypeScriptGeneratorNaming.determineValueName(name)).nn)
            w("\"")
            for param <- params do
              w(", ")
              param match {
                case MethodParameter.PositionalParam(name, paramType) =>
                  writeWrapCodecOfType(paramType)
                  w(".wrap(")
                  writeValueName(name)
                  w(")")
              }
            end for
            wl(");")
          }
          wl("}")

          w("return ")
          writeWrapCodecOfType(returnType)
          w(".unwrap(")
          if async then w("env.fromJSPromise(")
          w("esexpr_reserved_result")
          if async then w(")")
          wl(");")
        }
        wl("}")
    }

  def writeParameterCodec(param: Parameter): Unit =
    param match {
      case Parameter.KeywordParam(_, paramType, true, _) =>
        writeOptionalUndefinedCodec(paramType)

      case Parameter.KeywordParam(_, paramType, false, _) =>
        writeWrapCodecOfType(paramType)

      case Parameter.Dict(_, paramType) =>
        writeDictCodec(paramType)

      case Parameter.PositionalParam(_, paramType) =>
        writeWrapCodecOfType(paramType)

      case Parameter.VarArgParam(_, paramType) =>
        writeListCodec(paramType)
    }

  def writeOptionalUndefinedCodec(t: DataType): Unit =
    w(jsUtilPackageName)
    w(".WrapHelper.optionalUndefinedCodec(env, ")
    writeWrapCodecOfType(t)
    w(")")
  end writeOptionalUndefinedCodec

  def writeDictCodec(t: DataType): Unit =
    w(jsUtilPackageName)
    w(".WrapHelper.dictCodec(env, ")
    writeWrapCodecOfType(t)
    w(")")
  end writeDictCodec

  def writeListCodec(t: DataType): Unit =
    w(jsUtilPackageName)
    w(".WrapHelper.listCodec(env, ")
    writeWrapCodecOfType(t)
    w(")")
  end writeListCodec

  def writeWrapCodecOfType(t: DataType): Unit =
    t match {
      case DataType.UserDefined(name) if definitions.exists(GeneratorUtil.getDefinitionName(_) == name) =>
        w(packageName)
        w(".")
        writeTypeName(name + "-wrap-codec")
        w(".codec(env)")

      case DataType.UserDefined(name) =>
        writeValueName(name + "-wrap-codec")

      case DataType.Apply(name, args*) =>
        w(packageName)
        w(".")
        writeTypeName(name + "-wrap-codec")
        w(".codec(env")
        for arg <- args do
          w(", ")
          writeWrapCodecOfType(arg)
        end for
        w(")")

      case DataType.TypeEnumMatch(typeEnum, typeEnumValue, mappings) =>
        val typeEnumDef = definitions.find(GeneratorUtil.getDefinitionName(_) == typeEnum) match {
          case Some(t: Definition.TypeEnum) => t
          case Some(_) => throw new Exception(s"${typeEnum} is not a type enum")
          case None => throw new Exception(s"${typeEnum} is not defined")
        }

        writeWrapCodecOfType(typeEnumValue)
        w(".matchCodec(")
        writeCommaListSingleLine[String](value => {
          writeWrapCodecOfType(mappings(value))
        })(typeEnumDef.values)
        w(")")


      case DataType.TypeStructMember(_, typeStructValue, member) =>
        writeWrapCodecOfType(typeStructValue)
        w(".")
        writeValueName(member)
        w("Codec()")

      case DataType.List(elementType) =>
        writeListCodec(elementType)

      case DataType.Set(elementType) =>
        w(jsUtilPackageName)
        w(".WrapHelper.setCodec(env, ")
        writeWrapCodecOfType(elementType)
        w(")")

      case DataType.Dict(elementType) =>
        w(jsUtilPackageName)
        w(".WrapHelper.dictCodec(env, ")
        writeWrapCodecOfType(elementType)
        w(")")

      case DataType.Nullable(t) =>
        w(jsUtilPackageName)
        w(".WrapHelper.optionalNullCodec(env, ")
        writeWrapCodecOfType(t)
        w(")")

      case DataType.Bool =>
        w(jsUtilPackageName)
        w(".WrapHelper.boolCodec(env)")

      case DataType.IntType =>
        w(jsUtilPackageName)
        w(".WrapHelper.intCodec(env)")

      case DataType.UInt32 => ???
      case DataType.Int32 => ???
      case DataType.UInt64 => ???
      case DataType.Int64 => ???
      case DataType.Str =>
        w(jsUtilPackageName)
        w(".WrapHelper.strCodec(env)")

      case DataType.Binary => ???
      case DataType.Float32 => ???
      case DataType.Float64 => ???
      case DataType.ESExpr =>
        w(jsUtilPackageName)
        w(".WrapHelper.exprCodec(env)")

      case DataType.ESExprTag =>
        w(jsUtilPackageName)
        w(".WrapHelper.exprTagCodec(env)")
    }

}

object JavaRunJSGenerator {

  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.JavaRunJS): Task[Unit] =

    val convDefinitions = definitions
      .filter {
        case Definition.Const(_, _, _) => false
        case _: Definition.Extern => false
        case _ => true
      }

    ZIO.foreachDiscard(convDefinitions) { definition =>
      ZIO.attempt {
        val fileName = GeneratorBase.determineNamePascalCase(GeneratorUtil.getDefinitionName(definition))
        val outFile = outDir.resolve(config.outDir).nn.resolve(fileName + "WrapCodec.java").nn
        Files.createDirectories(outFile.getParent.nn)
        
        Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
          Using.resource(new PrintWriter(writer)) { writer =>
            val generator = JavaRunJSGenerator(writer, definitions, config.packageName, config.javaApiPackageName, config.jsUtilPackageName)
            generator.generateWrapCodec(definition)
          }
        }
      }
    }
  end generate
}
