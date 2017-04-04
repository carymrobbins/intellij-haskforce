package com.haskforce.project.template.stack

import javax.swing.Icon

import com.haskforce.HaskellIcons
import com.intellij.framework.FrameworkTypeEx
import com.intellij.framework.addSupport.FrameworkSupportInModuleProvider

class StackFrameworkType extends FrameworkTypeEx(StackFrameworkType.ID) {

  override def createProvider(): FrameworkSupportInModuleProvider = new StackFrameworkProvider

  override def getPresentableName: String = "Stack Framework"

  override def getIcon: Icon = HaskellIcons.FILE
}

object StackFrameworkType {
  val ID = "Haskell Stack Framework"
  val INSTANCE = FrameworkTypeEx.EP_NAME.findExtension(classOf[StackFrameworkType])
}