package dev.argon.plugins.js

import scala.jdk.CollectionConverters.given
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyObject}


private[js] type JSValue = ProxyObject | ProxyArray | Double | String | Boolean

object JSValue {
  def fromMap(m: Map[String, JSValue]): JSValue = ProxyObject.fromMap(m.asJava)
  def fromSeq(s: Seq[JSValue]): JSValue = ProxyArray.fromList(s.asJava)
}


