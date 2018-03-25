package com.haskforce.highlighting.annotation.external

final case class VersionTriple(major: Int, minor: Int, patch: Int) {

  def >(v: VersionTriple): Boolean = {
    major > v.major || major == v.major && (
      minor > v.minor || minor == v.minor && patch > v.patch
    )
  }

  def <(v: VersionTriple): Boolean = {
    major < v.major || major == v.major && (
      minor < v.minor || minor == v.minor && patch < v.patch
    )
  }

  def >=(v: VersionTriple): Boolean = {
    this == v || >(v)
  }

  def <=(v: VersionTriple): Boolean = {
    this == v || <(v)
  }
}
