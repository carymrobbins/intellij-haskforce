package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.ExternalProjectSettings

// NOTE: If any fields are added to this class, you will need to update
// the 'equals' method to compare the fields accordingly.
final class StackProjectSettings extends ExternalProjectSettings {

  override def clone(): StackProjectSettings = {
    val s = new StackProjectSettings
    copyTo(s)
    s
  }

  override def equals(obj: Any): Boolean = {
    if (!super.equals(obj)) return false
    // If any other fields are added, compare them here.
    // It might be advantageous to contain them within some inner
    // 'case class StackProjectSettings.State(...)' so we can
    // compare them here with 'this.state == obj.state'.
    true
  }
}

object StackProjectSettings {

  def of(externalProjectPath: String): StackProjectSettings = {
    val s = new StackProjectSettings
    s.setExternalProjectPath(externalProjectPath)
    s
  }
}
