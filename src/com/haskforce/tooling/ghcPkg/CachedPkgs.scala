package com.haskforce.tooling.ghcPkg

final class CachedPkgs(
  val toMap: Map[String, CachedPkgs.Versions]
) extends AnyVal {

  def firstNamed(name: String): Option[Pkg] = named(name).flatMap(_.head)

  def named(name: String): Option[CachedPkgs.Versions] = toMap.get(name)

  def add(pkg: Pkg): CachedPkgs = {
    new CachedPkgs(
      toMap.updated(
        pkg.name,
        toMap.getOrElse(pkg.name, CachedPkgs.Versions.empty).add(pkg)
      )
    )
  }
}

object CachedPkgs {

  def empty: CachedPkgs = new CachedPkgs(Map.empty)

  def fromIterator(it: Iterator[Pkg]): CachedPkgs = {
    it.foldLeft(empty)((pkgs, pkg) => pkgs.add(pkg))
  }

  final class Versions(val toMap: Map[String, Pkg]) extends AnyVal {
    def versioned(version: String): Option[Pkg] = toMap.get(version)

    def add(pkg: Pkg): Versions = {
      new Versions(toMap.updated(pkg.version, pkg))
    }

    def head: Option[Pkg] = toMap.headOption.map(_._2)
  }

  object Versions {
    def empty: Versions = new Versions(Map.empty)
  }
}
