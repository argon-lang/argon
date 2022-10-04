package dev.argon.vm.engine

import scala.scalajs.js

final class VMException(cause: js.Error) extends Exception
type VMFormatException = VMException
