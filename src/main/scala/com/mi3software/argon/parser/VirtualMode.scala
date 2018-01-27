package com.mi3software.argon.parser


sealed trait VirtualMode {
  def isVirtual: Boolean
  def isAbstract: Boolean
  def isOverride: Boolean
  def isFinal: Boolean
}

object VirtualMode {
  case object NonVirtual extends VirtualMode {
    override def isVirtual: Boolean = false
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false
    override def isFinal: Boolean = false
  }

  case object Virtual extends VirtualMode {
    override def isVirtual: Boolean = true
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false
    override def isFinal: Boolean = false
  }

  case object Abstract extends VirtualMode {
    override def isVirtual: Boolean = true
    override def isAbstract: Boolean = true
    override def isOverride: Boolean = false
    override def isFinal: Boolean = false
  }

  case object Override extends VirtualMode {
    override def isVirtual: Boolean = true
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = true
    override def isFinal: Boolean = false
  }

  case object AbstractOverride extends VirtualMode {
    override def isVirtual: Boolean = true
    override def isAbstract: Boolean = true
    override def isOverride: Boolean = true
    override def isFinal: Boolean = false
  }

  case object Final extends VirtualMode {
    override def isVirtual: Boolean = false
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false
    override def isFinal: Boolean = true
  }

  case object FinalOverride extends VirtualMode {
    override def isVirtual: Boolean = true
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = true
    override def isFinal: Boolean = true
  }
}
