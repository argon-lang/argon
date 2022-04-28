package dev.argon.util.xml

final case class Name(namespace: Namespace, name: String) derives CanEqual

object Name {
  def apply(name: String): Name = Name(Namespace(""), name)
}

