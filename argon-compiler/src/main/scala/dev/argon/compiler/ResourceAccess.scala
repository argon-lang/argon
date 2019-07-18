package dev.argon.compiler

import cats.data.NonEmptyList
import dev.argon.compiler.core._
import dev.argon.stream.{ArStream, Resource, StreamTransformation, ZipEntryInfo}
import dev.argon.stream.{Resource, StreamTransformation, ZipEntryInfo}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait ResourceAccess[TContext <: Context with Singleton] {

  val context: TContext

  import context._

  def getExtension(id: ResIndicator): CompE[NonEmptyList[CompilationError], String]

  def resourceSink(id: ResIndicator): Resource[CompRE, Environment, NonEmptyList[CompilationError], StreamTransformation[CompRE, Environment, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]]
  def zipFromEntries(entryStream: ArStream[CompRE, Environment, NonEmptyList[CompilationError], ZipEntryInfo[CompRE, Environment, NonEmptyList[CompilationError]]]): ArStream[CompRE, Environment, NonEmptyList[CompilationError], Byte]

  type ZipReader
  def getZipReader[A](id: ResIndicator): Resource[CompRE, Environment, NonEmptyList[CompilationError], ZipReader]
  def zipEntryStream(zip: ZipReader, name: String): ArStream[CompRE, Environment, NonEmptyList[CompilationError], Byte]

  def protocolBufferSink[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A]): StreamTransformation[CompRE, Environment, NonEmptyList[CompilationError], Byte, Unit, Nothing, A]
  def protocolBufferStream(message: GeneratedMessage): ArStream[CompRE, Environment, NonEmptyList[CompilationError], Byte]
}

trait ResourceAccessFactory[-TContext <: Context] {

  def create(context: TContext): ResourceAccess[context.type]

}
