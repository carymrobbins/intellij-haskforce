package com.haskforce.eta.run

import prelude._

import java.io.File

import com.haskforce.cabal.completion.CabalFileFinder
import com.haskforce.cabal.query.{BuildInfo, CabalQuery}
import com.haskforce.psi.HaskellFile
import com.haskforce.utils.{FileUtil, IJReadAction}
import com.intellij.execution.actions.{ConfigurationContext, RunConfigurationProducer}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Ref
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
    getResultForContext(context).fold(false) { data =>
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
  ): Boolean = getResultForContext(context).isDefined
}

object EtlasRunConfigurationProducer {

  final case class Result(
    name: String,
    workingDirectory: String
  )

  def getResultForContext(context: ConfigurationContext): Option[Result] = {
    for {
      location <- Option(context.getPsiLocation)
      haskellPsiFile <- Option(location.getContainingFile).flatMap(classTag[HaskellFile].unapply)
      haskellVFile <- Option(haskellPsiFile.getVirtualFile)
      cabalPsiFile <- CabalFileFinder.psiForFile(haskellPsiFile).run(context)
      cabalVFile <- Option(cabalPsiFile.getVirtualFile)
      cabalQuery = new CabalQuery(SPsiFile(cabalPsiFile))
      configState <- cabalQuery.getExecutables.run(context).flatMap(getConfigState(
        cabalVFile.getParent.getCanonicalPath,
        haskellVFile.getCanonicalPath,
        _
      ).run(context)).headOption
    } yield configState
  }

  private def getConfigState(
    cabalDirPath: String,
    mainFilePath: String,
    ex: BuildInfo.Executable
  ): IJReadAction[Option[Result]] = {
    for {
      name <- OptionT(ex.getName)
      mainPath <- OptionT(ex.getMainIs)
      _ <- ex.getSourceDirs.liftM[OptionT].filter(_.toSet.exists(d =>
        new File(FileUtil.join(cabalDirPath, d, mainPath)).getCanonicalPath == mainFilePath
      ))
    } yield Result(name, cabalDirPath)
  }.run
}
