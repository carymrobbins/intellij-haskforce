package com.haskforce.eta.jps.model

import org.jetbrains.jps.model.JpsProject
import org.jetbrains.jps.model.ex.{JpsCompositeElementBase, JpsElementChildRoleBase}

/**
 * Created by cary.robbins on 12/17/16.
 */
class JpsEtaBuildOptionsExtension(
  private var options: EtaBuildOptions
) extends JpsCompositeElementBase[JpsEtaBuildOptionsExtension] {

  override def createCopy(): JpsEtaBuildOptionsExtension = {
    new JpsEtaBuildOptionsExtension(options.copy())
  }

  def getOptions: EtaBuildOptions = options

  def setOptions(options: EtaBuildOptions): Unit = { this.options = options }

  override def toString: String = s"JpsEtaBuildOptionsExtension{options=$options}"
}

object JpsEtaBuildOptionsExtension {

  val ROLE = JpsElementChildRoleBase.create[JpsEtaBuildOptionsExtension]("EtaBuildOptions")

  def getOrCreate(project: JpsProject): JpsEtaBuildOptionsExtension = {
    Option(project.getContainer.getChild(ROLE)).getOrElse {
      project.getContainer.setChild(ROLE, new JpsEtaBuildOptionsExtension(new EtaBuildOptions))
    }
  }
}
