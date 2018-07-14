package com.mi3software.argon

import com.mi3software.argon.module.Module
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TSimpleFileTransport

package object librarygen {

  val moduleFormatVersion = 1

  def writeModule(fileName: String, module: Module): Unit = {
    val trans = new TSimpleFileTransport(fileName, false, true)
    val prot = new TBinaryProtocol(trans)
    module.write(prot)

    trans.close()
  }

}
