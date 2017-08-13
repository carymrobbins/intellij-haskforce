package com.haskforce.eta.run

import java.io.File

import com.haskforce.cabal.completion.CabalFileFinder
import com.haskforce.cabal.query.{BuildInfo, CabalQuery}
import com.haskforce.psi.HaskellFile
import com.haskforce.utils.FileUtil
import com.intellij.execution.actions.{ConfigurationContext, RunConfigurationProducer}
import com.intellij.execution.configuration.EnvironmentVariablesData
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.util.Ref
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement

import scala.reflect.classTag

class EtlasRunConfigurationProducer extends RunConfigurationProducer[EtlasRunConfiguration](
  EtlasConfigurationType.INSTANCE
) {

  import EtlasRunConfigurationProducer._

  override def setupConfigurationFromContext(
    configuration: EtlasRunConfiguration,
    context: ConfigurationContext,
    sourceElement: Ref[PsiElement]
  ): Boolean = {
    getMetaDataForContext(context).fold(false) { data =>
      configuration.setName(data.name)
      configuration.updateConfigState(_.copy(
        programArguments = s"run ${data.name}",
        workingDirectory = data.workingDirectory
      ))
      true
    }
  }

  override def isConfigurationFromContext(
    configuration: EtlasRunConfiguration,
    context: ConfigurationContext
  ): Boolean = getMetaDataForContext(context).isDefined
}

object EtlasRunConfigurationProducer {

  final case class MetaData(
    name: String,
    workingDirectory: String
  )

  def getMetaDataForContext(context: ConfigurationContext): Option[MetaData] = {
    for {
      location <- Option(context.getPsiLocation)
      haskellPsiFile <- Option(location.getContainingFile).flatMap(classTag[HaskellFile].unapply)
      haskellVFile <- Option(haskellPsiFile.getVirtualFile)
      cabalPsiFile <- CabalFileFinder.psiForFile(haskellPsiFile)
      cabalVFile <- Option(cabalPsiFile.getVirtualFile)
      cabalQuery = new CabalQuery(cabalPsiFile)
      configState <- cabalQuery.getExecutables.flatMap(getConfigState(
        cabalVFile.getParent.getCanonicalPath,
        haskellVFile.getCanonicalPath,
        _
      )).headOption
    } yield configState
  }

  def getConfigState(
    cabalDirPath: String,
    mainFilePath: String,
    ex: BuildInfo.Executable
  ): Option[MetaData] = for {
    name <- ex.getName
    mainPath <- ex.getMainIs
    if ex.getSourceDirs.toSet.exists(d =>
      new File(FileUtil.join(cabalDirPath, d, mainPath)).getCanonicalPath == mainFilePath
    )
  } yield MetaData(name, cabalDirPath)
}
