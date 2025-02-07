package dev.argon.build


final case class CompileOptions(
  tubeName: String,
  inputDir: String,
  referencedTubes: Seq[String],
  outputFile: String,
)
