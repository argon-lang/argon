package dev.argon.plugin

import dev.argon.io.{Resource, ResourceTag}
import zio.IO

import java.io.IOException
import scala.reflect.TypeTest

trait BuildOutputExecutor {
  type BuildOutput <: Resource
  type BuildOutputTag <: ResourceTag[BuildOutput]
  given buildOutputTagTypeTest: TypeTest[ResourceTag[BuildOutput], BuildOutputTag]

  def execute(buildOutput: BuildOutput): IO[IOException, String]
}
