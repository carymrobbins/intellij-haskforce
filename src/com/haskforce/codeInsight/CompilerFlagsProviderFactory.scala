package com.haskforce.codeInsight

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.psi.PsiFile

import com.haskforce.highlighting.annotation.external.{GhcMod, GhcModi}
import com.haskforce.utils.ExecUtil

object CompilerFlagsProviderFactory {
  def get(psiFile: PsiFile): Option[CompilerFlagsProvider] = {
    GhcModiCompilerFlagsProvider.create(psiFile).orElse(
      GhcModCompilerFlagsProvider.create(psiFile)
    )
  }
}

trait CompilerFlagsProvider {
  def getFlags: Array[String]
}

class GhcModiCompilerFlagsProvider(
  ghcModi: GhcModi
) extends CompilerFlagsProvider {

  override def getFlags: Array[String] = {
    Option(ghcModi.syncFlag()).getOrElse(Array.empty)
  }
}

object GhcModiCompilerFlagsProvider {
  def create(psiFile: PsiFile): Option[GhcModiCompilerFlagsProvider] = for {
    ghcModi <- GhcModi.get(psiFile)
  } yield new GhcModiCompilerFlagsProvider(ghcModi)
}

class GhcModCompilerFlagsProvider(
  module: Module,
  workDir: String
) extends CompilerFlagsProvider {

  override def getFlags: Array[String] = {
    Option(GhcMod.flag(module, workDir)).getOrElse(Array.empty)
  }
}

object GhcModCompilerFlagsProvider {
  def create(psiFile: PsiFile): Option[GhcModCompilerFlagsProvider] = for {
    module <- Option(ModuleUtilCore.findModuleForPsiElement(psiFile))
    workDir <- Option(ExecUtil.guessWorkDir(module))
  } yield new GhcModCompilerFlagsProvider(module, workDir)
}
