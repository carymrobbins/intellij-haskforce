package com.haskforce.tools.ghcmod.modi.codeInsight

import com.haskforce.system.integrations.codeinsight.VisibleModulesProvider
import com.haskforce.tools.ghcmod.modi.GhcModi
import com.intellij.psi.PsiFile

class GhcModiVisibleModulesProvider(ghcModi: GhcModi) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(ghcModi.syncList()).getOrElse(Array.empty)
  }
}

object GhcModiVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModiVisibleModulesProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiVisibleModulesProvider(ghcModi)
}
