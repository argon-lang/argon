package dev.argon.vm.interpreter

import dev.argon.vm.{ClassId, FieldId}

type GCObjectNotNull = BigInt | String | GCObjectMap
type GCObject = GCObjectNotNull | Null
final case class GCObjectMap(instanceClass: ClassId, fields: Map[FieldId, VMCell], arrayElements: Seq[VMCell])



