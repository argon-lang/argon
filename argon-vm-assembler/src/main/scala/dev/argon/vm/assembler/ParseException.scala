package dev.argon.vm.assembler

class ParseException(message: String | Null) extends Exception(message) {
	def this() = this(null)
}
