package com.haskforce.tools.stack.importWizard

import java.io.{File, Serializable}
import java.util
import javax.swing.Icon

import com.haskforce.Implicits._
import com.haskforce.haskell.HaskellSdkType
import com.haskforce.haskell.ui.HaskellIcons
import com.haskforce.importWizard.stack.StackYaml
import com.haskforce.system.packages._
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.{FileUtil, NotificationUtil, SAMUtils}
import com.haskforce.tools.stack.packages.{StackPackage, StackPackageManager, StackProjectManager}
import com.intellij.notification.NotificationType
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{ModifiableModuleModel, Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.startup.StartupManager
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile, VirtualFileSystem}
import com.intellij.packaging.artifacts.ModifiableArtifactModel
import com.intellij.projectImport.ProjectImportBuilder

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * Imports a Stack project and configures modules from the stack.yaml file.
 */
class StackProjectImportBuilder extends ProjectImportBuilder[StackYaml.Package] {
  override def getName: String = "Stack"
  override def getIcon: Icon = HaskellIcons.FILE

  sealed case class Params(
    var stackPath: Option[String] = None,
    var stackYamlPath: Option[String] = None,
    var packages: util.List[StackYaml.Package] = util.Collections.emptyList(),
    var openSettings: Boolean = false
  )

  val params = Params()

  override def getList: util.List[StackYaml.Package] = params.packages
  override def setList(list: util.List[StackYaml.Package]): Unit = params.packages = list
  override def isOpenProjectSettingsAfter: Boolean = params.openSettings
  override def setOpenProjectSettingsAfter(on: Boolean): Unit = params.openSettings = on
  override def isMarked(element: StackYaml.Package): Boolean = getList.contains(element)

  override def commit(
      project: Project,
      model: ModifiableModuleModel,
      modulesProvider: ModulesProvider,
      artifactModel: ModifiableArtifactModel): util.List[Module] = {
    val stackPath = unsafeGetParam(_.stackPath, "stackPath")
    val stackYamlPath = unsafeGetParam(_.stackYamlPath, "stackYamlPath")
    //provided model may be null
    val moduleModel = Option(model).getOrElse {
      ModuleManager.getInstance(project).getModifiableModel
    }
    val packagesOpt: Option[(VirtualFile, List[ChangedModule])] = ApplicationManager.getApplication.runWriteAction { () =>
      commitSdk(project)
      val packagesOpt = initPackages(stackYamlPath, project, moduleModel)
      if (packagesOpt.isDefined) {
        setProjectSettings(project, stackPath, stackYamlPath)
      }
      packagesOpt
    }
    packagesOpt.map(tuple => {
      val (stackFile, changedModules) = tuple
      StackProjectManager.getInstance(project).addStackFile(stackFile)
      changedModules.map(_.module).asJava
    }).getOrElse(new util.ArrayList[Module]())
  }

  private def setProjectSettings(
      project: Project,
      stackPath: String,
      stackYamlPath: String): Unit = {
    val buildSettings = HaskellBuildSettings.getInstance(project)
    buildSettings.setUseCabal(false)
    buildSettings.setUseStack(true)
    buildSettings.setStackPath(stackPath)
    buildSettings.setStackFile(stackYamlPath)
  }

  private def initPackages(stackYamlPath: String, project: Project, model: ModifiableModuleModel): Option[(VirtualFile, List[ChangedModule])] = {
    val projectRoot = getImportRoot
    val virtualStackFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(new File(stackYamlPath))
    if (virtualStackFile == null) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, project,
        "unable to set Haskell Compiler", s"unable to obtain VirtualFile for $stackYamlPath"
      )
      return None
    }
    val result = StackPackageManager.getPackages(virtualStackFile, project)
    if (result.isLeft) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, project,
        "unable to set Haskell Compiler", s"unable to parse $stackYamlPath, error: ${result.left.get.errorMsg}"
      )
      None
    } else {
      val errors: List[FileError] = result.right.get
        .flatMap(_.left.toOption)
      if (errors.nonEmpty) {
        val messages: String = errors
          .map(error => s"in package: ${error.fileName} error: ${error.errorMsg}")
          .mkString("<br/>")
        NotificationUtil.displaySimpleNotification(
          NotificationType.ERROR, project,
          "unable to register all packages", messages
        )
      }

      val marked: Set[VirtualFile] = getList.flatMap(pkg => FileUtil.fromRelativePath(pkg.path, new File(stackYamlPath).getParent)).toSet

      val chosenPackages: List[HPackage] = result.right.get
        .flatMap(_.right.toOption)
        .filter(pkg => marked.contains(pkg.getLocation.getParent))

      if (chosenPackages.nonEmpty) {
        val (shadowed, updated, created) =
          ProjectSetup.setUpWithUpdate(chosenPackages, project, model, projectRoot, setupRoot = !isUpdate)

        if (shadowed.nonEmpty) {
          NotificationUtil.displaySimpleNotification(
            NotificationType.ERROR, project,
            "unable to register all packages", s"some packages are shadowed by existing packages:<br/>" + shadowed
              .map(shadowedPackage => s"sourceDir ${shadowedPackage.matchingSourceDir} from package ${shadowedPackage.module}" +
                s"is shadowed by content-root ${shadowedPackage.matchingContentRoot} of module ${shadowedPackage.module}")
              .mkString("<br/>")
          )
        }

        val changedModules: List[ChangedModule] = created ++ updated
        Some((virtualStackFile, changedModules))
      } else {
        NotificationUtil.displaySimpleNotification(
          NotificationType.ERROR, project,
          "unable to set Haskell Compiler", s"unable to set up any Package"
        )
        None
      }
    }
  }

  // Helper to be called only after validation.
  private def unsafeGetParam[A](f: Params => Option[A], name: String): A = {
    f(params).getOrElse { throw new RuntimeException(s"Parameter '$name' not set.") }
  }

  private def commitSdk(project: Project): Unit = {
    ProjectRootManager.getInstance(project).setProjectSdk(findOrCreateSdk())
  }

  private def findOrCreateSdk(): Sdk = {
    val sdkType = HaskellSdkType.getInstance
    // Essentially, sorts the Sdks so that the Haskell one comes first.
    // If it doesn't exist, should create a new one.
    val cmp = { (sdk1: Sdk, sdk2: Sdk) =>
      if (sdk1.getSdkType == sdkType) -1
      else if (sdk2.getSdkType == sdkType) 1
      else 0
    }
    SdkConfigurationUtil.findOrCreateSdk(cmp, sdkType)
  }

  def getImportRoot: String = {
    val path = getFileToImport
    val f = new File(path)
    if (f.isDirectory) path else f.getParent
  }
}

object StackProjectImportBuilder {
  private val LOG = Logger.getInstance(classOf[StackProjectImportBuilder])
}
