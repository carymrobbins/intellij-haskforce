package com.haskforce.tooling.ghcPkg

import java.util

import scala.beans.BeanProperty

final case class Pkg(
  @BeanProperty var name: String,
  @BeanProperty var version: String,
  @BeanProperty var exposedModules: util.Set[String]
) extends Comparable[Pkg] {

  def this() = this(null, null, null)

  override def compareTo(o: Pkg): Int = {
    Pkg.ordering.compare(this, o)
  }
}

object Pkg {
  implicit val ordering: Ordering[Pkg] = Ordering.by { x: Pkg => (x.name, x.version) }
}
