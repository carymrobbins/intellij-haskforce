package com.haskforce.utils

import java.net.ServerSocket

object PortUtil {
  def findFreePort(): Int = {
    val s = new ServerSocket(0)
    val port = s.getLocalPort
    s.close()
    port
  }
}
