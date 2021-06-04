package com.haskforce.tooling.stack

import com.haskforce.haskell.project.externalSystem.stack.PackageConfigAssoc
import com.haskforce.utils.JDOMExternalizable
import com.intellij.openapi.components.{PersistentStateComponent, State, Storage}
import com.intellij.openapi.project.Project
import org.jdom.Element

@State(
  name = "PackageConfigCacheService",
  storages = Array(new Storage("package-configs.xml"))
)
class PackageConfigCacheService extends PersistentStateComponent[Element] {

  private var state = PackageConfigCacheService.State(Nil)

  override def getState: Element = {
    val el = new Element("State")
    JDOMExternalizable.writeExternal(el, state)
    el
  }

  override def loadState(el: Element): Unit = {
    this.state = JDOMExternalizable.readExternal[PackageConfigCacheService.State](el)
  }

  def setPackageConfigAssocs(x: List[PackageConfigAssoc]): Unit = {
    state.packageConfigAssocs = x
  }

  def getPackageConfigAssocs: List[PackageConfigAssoc] = {
    state.packageConfigAssocs
  }
}

object PackageConfigCacheService {

  def getInstance(project: Project): PackageConfigCacheService = {
    project.getService(classOf[PackageConfigCacheService])
  }

  final case class State(
    var packageConfigAssocs: List[PackageConfigAssoc]
  )

  object State {

    implicit val jdom: JDOMExternalizable[State] =
      JDOMExternalizable.derive1(apply, unapply)
  }
}
