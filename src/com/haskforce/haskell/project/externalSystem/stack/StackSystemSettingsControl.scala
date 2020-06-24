package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.util.{ExternalSystemSettingsControl, PaintAwarePanel}

class StackSystemSettingsControl
  extends ExternalSystemSettingsControl[StackSettings] {

  override def fillUi(canvas: PaintAwarePanel, indentLevel: Int): Unit = {}

  override def reset(): Unit = {}

  override def isModified: Boolean = false

  override def apply(settings: StackSettings): Unit = {}

  override def validate(settings: StackSettings): Boolean = true

  override def disposeUIResources(): Unit = {}

  override def showUi(show: Boolean): Unit = {}
}
