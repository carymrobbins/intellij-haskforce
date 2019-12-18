package com.haskforce.highlighting.annotation.external.hsdev

import com.intellij.openapi.module.{Module, ModuleComponent, ModuleUtilCore}
import com.intellij.psi.PsiElement

class HsDevModuleComponent extends ModuleComponent {
  val cache = HsDevCache()
}

object HsDevModuleComponent {
  def get(module: Module): Option[HsDevModuleComponent] = {
    Option(module.getComponent(classOf[HsDevModuleComponent]))
  }

  def get(element: PsiElement): Option[HsDevModuleComponent] = {
    Option(ModuleUtilCore.findModuleForPsiElement(element)).flatMap(get)
  }
}
