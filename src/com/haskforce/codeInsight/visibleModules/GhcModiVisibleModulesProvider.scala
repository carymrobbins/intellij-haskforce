package com.haskforce.codeInsight.visibleModules

import com.haskforce.highlighting.annotation.external.GhcModi
import com.intellij.psi.PsiFile

class GhcModiVisibleModulesProvider(
  ghcModi: GhcModi
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    Option(ghcModi.syncList()).getOrElse(Array.empty)
  }
}

object GhcModiVisibleModulesProvider {
  def create(psiFile: PsiFile): Option[GhcModiVisibleModulesProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiVisibleModulesProvider(ghcModi)
}
