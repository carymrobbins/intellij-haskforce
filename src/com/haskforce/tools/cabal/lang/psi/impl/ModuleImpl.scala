package com.haskforce.tools.cabal.lang.psi.impl

import com.intellij.psi.PsiElement

import com.haskforce.tools.cabal.lang.psi.ModulePart

trait ModuleImpl extends PsiElement {

  def getParts: Array[ModulePart] = getChildren.map(assertModulePart)

  def getFirstPart: ModulePart = assertModulePart(getFirstChild)

  def getLastPart: ModulePart = assertModulePart(getLastChild)

  private def assertModulePart(el: PsiElement): ModulePart = el match {
    case el: ModulePart => el
    case other => throw new CabalElementTypeError("ModulePart", other)
  }
}
