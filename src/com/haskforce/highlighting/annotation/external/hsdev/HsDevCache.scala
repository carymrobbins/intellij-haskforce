package com.haskforce.highlighting.annotation.external.hsdev

// TODO: We need TTLs on the cache fields.
final class HsDevCache {
  var port: Option[Int] = None
  var scanned: HsDevCache.ScanState = HsDevCache.ScanState.NotScanned
  var installedModules: Option[Vector[HsDevModule]] = None

  def clear(): Unit = {
    port = None
    scanned = HsDevCache.ScanState.NotScanned
    installedModules = None
  }
}

object HsDevCache {

  sealed trait ScanState
  object ScanState {
    case object NotScanned extends ScanState
    case object Scanned extends ScanState
    case object ScanFailure extends ScanState
  }
}
