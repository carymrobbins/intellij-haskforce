package com.haskforce.tooling.ghcPkg

import java.util

import scala.beans.BeanProperty

final class CachedPkgs(
  @BeanProperty var internal: util.Map[String, CachedPkgs.Versions]
) {

  def this() = this(new util.HashMap())

  def firstNamed(name: String): Option[Pkg] = named(name).flatMap(_.headOption)

  def named(name: String): Option[CachedPkgs.Versions] = Option(internal.get(name))

  def add(pkg: Pkg): Unit = {
    internal.computeIfAbsent(pkg.name, _ => new CachedPkgs.Versions).add(pkg)
  }
}

object CachedPkgs {

  def fromIterator(it: Iterator[Pkg]): CachedPkgs = {
    val res = new CachedPkgs
    it.foreach(res.add)
    res
  }

  final class Versions(
    @BeanProperty var internal: util.Map[String, Pkg]
  ) {

    def this() = this(new util.HashMap())

    def versioned(version: String): Option[Pkg] = Option(internal.get(version))

    def add(pkg: Pkg): Unit = {
      internal.put(pkg.version, pkg)
      ()
    }

    def headOption: Option[Pkg] = {
      val it = internal.entrySet().iterator()
      if (!it.hasNext) return None
      Option(it.next().getValue)
    }
  }
}
