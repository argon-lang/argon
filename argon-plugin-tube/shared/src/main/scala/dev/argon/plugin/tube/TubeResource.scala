package dev.argon.plugin.tube

import dev.argon.io.{ResourceTag, ZipFileResource}
import dev.argon.plugin.SerializedTube
import zio.Managed
import java.io.IOException

trait TubeResource extends ZipFileResource {
  def tube: Managed[IOException, SerializedTube]
}

object TubeResourceTag extends ResourceTag[TubeResource]

trait TubeImplementationResource extends TubeResource

object TubeImplementationResourceTag extends ResourceTag[TubeImplementationResource]
