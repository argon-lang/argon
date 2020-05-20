package dev.argon.compiler.options

trait PrimitiveCodecs[Codec[_], I] {
  def stringCodec: Codec[String]
  def resourceIndicatorCodec: Codec[I]
  def fileListCodec[A](codec: Codec[A]): Codec[FileList[A]]
  def singleFileCodec[A](codec: Codec[A]): Codec[SingleFile[A]]
  def optionCodec[A](codec: Codec[A]): Codec[Option[A]]
}
