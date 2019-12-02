package com.haskforce.highlighting.annotation.external.hsdev

// TODO: We need TTLs on the cache fields.
final class HsDevCache {
  var port: Option[Int] = None
  var scanned = false
  var installedModules: Option[Vector[HsDevModule]] = None

  def clear(): Unit = {
    port = None
    scanned = false
    installedModules = None
  }
}
