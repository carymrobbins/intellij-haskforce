package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, scanJsonArrayFromStream}
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement

import scala.collection.mutable

class HsDevExecutor(
  project: Project,
  module: Module,
  exeSettings: HsDevExeSettings,
  port: Int,
  cache: HsDevCache,
  useStack: Boolean
) {
  def installedModules: Vector[HsDevModule] = {
    cache.installedModules.getOrElse {
      val res = exec[HsDevModule]("module", "--installed")
      cache.installedModules = Some(res)
      res
    }
  }

  private def mkHsdevCli(): GeneralCommandLine = {
    val cli = exeSettings.toGeneralCommandLine
    cli.addParameters("--port", port.toString)
    cli
  }

  private def ensureScanned(): Unit = {
    val cli = mkHsdevCli()
    cli.addParameter("scan", "--project")
  }

  private def exec[A : JsonValueCodec](command: String, args: String*): Vector[A] = {
    ensureScanned()
    val cli = mkHsdevCli()
    cli.addParameter(command)
    cli.addParameters(args: _*)
    val proc = cli.createProcess()
    val stdout = proc.getInputStream
    val buf = new mutable.ArrayBuffer[A]
    scanJsonArrayFromStream(stdout) { a: A => buf += a ; true }
    buf.toVector
  }
}

object HsDevExecutor {
  def get(element: PsiElement): Option[HsDevExecutor] = {
    val project = element.getProject
    for {
      module <- Option(ModuleUtilCore.findModuleForPsiElement(element))
      projectComponent <- HsDevProjectComponent.get(project)
      exeParams <- projectComponent.getExeParams
      port <- projectComponent.currentPort
      moduleComponent <- HsDevModuleComponent.get(module)
      cache = moduleComponent.cache
    } yield new HsDevExecutor(project, module, exeParams, port, cache)
  }
}
