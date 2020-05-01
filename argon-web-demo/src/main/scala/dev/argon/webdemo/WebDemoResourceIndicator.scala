package dev.argon.webdemo

import java.net.URI

import dev.argon.compiler.loaders.ResourceIndicator
import WebDemoResourceIndicator.getExtension

sealed trait WebDemoResourceIndicator extends ResourceIndicator
final case class UriResourceIndicator(uri: String) extends WebDemoResourceIndicator {
  override def extension: String = getExtension(uri)

  override def show: String = uri.toString
}

final case class LocalResourceIndicator(name: String, content: String) extends WebDemoResourceIndicator {
  override def extension: String = getExtension(name)

  override def show: String = name
}

object WebDemoResourceIndicator {

  def getExtension(path: String): String = {
    val fileNameStart = path.lastIndexOf('/')
    val fileExtStart = path.lastIndexOf('.', fileNameStart)

    if(fileExtStart <= fileNameStart) ""
    else path.substring(fileExtStart + 1)
  }

}
