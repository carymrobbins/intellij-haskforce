package com.haskforce.highlighting.annotation.external.hsdev

// TODO: We need TTLs on the cache fields.
final class HsDevCache {
  var installedModules: Option[Vector[HsDevModule]] = None

  def clear(): Unit = {
    installedModules = None
  }
}
