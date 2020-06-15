package com.haskforce.haskell.project.externalSystem.stack

import com.haskforce.HaskellIcons
import com.intellij.openapi.externalSystem.ui.ExternalSystemIconProvider
import javax.swing.Icon

class StackIconProvider extends ExternalSystemIconProvider {
  override def getReloadIcon: Icon = HaskellIcons.FILE
}
