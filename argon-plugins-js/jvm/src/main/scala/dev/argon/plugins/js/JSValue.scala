package dev.argon.plugins.js

import scala.jdk.CollectionConverters.given
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyObject}


private[js] type JSValue = com.oracle.truffle.js.runtime.objects.JSDynamicObject | ProxyObject | ProxyArray | Double | String | Boolean | com.oracle.truffle.js.runtime.BigInt

object JSValue {
  def fromMap(m: Map[String, JSValue]): JSValue = ProxyObject.fromMap(m.asJava)
  def fromSeq(s: Seq[JSValue]): JSValue = ProxyArray.fromList(s.asJava)
  def fromBigInt(i: BigInt): JSValue = com.oracle.truffle.js.runtime.BigInt.fromBigInteger(i.bigInteger)

  def nullValue: JSValue = com.oracle.truffle.js.runtime.objects.Null.instance
}


