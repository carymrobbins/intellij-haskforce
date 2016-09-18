package com.haskforce.tools.stack.importWizard

import java.io.File
import java.util
import javax.swing.Icon

import com.haskforce.Implicits._
import com.haskforce.tools.cabal.query.CabalQuery
//TODO refactor
import com.haskforce.haskell.HaskellIcons
import com.haskforce.importWizard.stack.{StackYaml, StackYamlUtil}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.haskell.{HaskellModuleBuilder, HaskellModuleType, HaskellSdkType}
import com.intellij.ide.util.projectWizard.ModuleBuilder
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{ModifiableModuleModel, Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.roots.{ModifiableRootModel, ProjectRootManager}
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.VirtualFileManager
import com.intellij.packaging.artifacts.ModifiableArtifactModel
import com.intellij.projectImport.ProjectImportBuilder

import scala.collection.JavaConversions._

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
    val stackYaml = StackYaml.unsafeFromFile(stackYamlPath)
    val moduleModel = Option(model).getOrElse {
      ModuleManager.getInstance(project).getModifiableModel
    }
    ApplicationManager.getApplication.runWriteAction { () =>
      commitSdk(project)
      setProjectSettings(project, stackPath, stackYamlPath)
      buildModules(project, moduleModel, stackYaml)
    }
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

  private def buildModules(
      project: Project,
      moduleModel: ModifiableModuleModel,
      stackYaml: StackYaml): util.List[Module] = {
    val projectRoot = getImportRoot
    val (moduleDirs, modules) = stackYaml.packages.collect { case pkg if isMarked(pkg) =>
      buildStackPackageModule(project, moduleModel, projectRoot, pkg)
    }.unzip
    // If we aren't updating an existing project AND the project root isn't a module,
    // let's create it as the "root" module.
    if (!isUpdate && !moduleDirs.contains(projectRoot)) {
      val projectName = new File(projectRoot).getName
      val projectModule = buildModule(project, moduleModel, projectRoot, projectName + " (root)", None)
      modules.add(projectModule)
    }
    modules
  }

  /**
   * Builds an intellij module for a given package.  Must be run within a write action.
   */
  private def buildStackPackageModule(
      project: Project,
      moduleModel: ModifiableModuleModel,
      projectRoot: String,
      pkg: StackYaml.Package): (String, Module) = {
    val moduleDir = new File(FileUtil.join(projectRoot, pkg.path)).getCanonicalPath
    // This should already be validated.
    val cabalFile = StackYamlUtil.unsafeFindCabalFile(projectRoot, pkg)
    val moduleName = cabalFile.getName.split('.').head
    val optCabalQuery = CabalQuery.fromJavaFile(None, cabalFile)
    val module = buildModule(project, moduleModel, moduleDir, moduleName, optCabalQuery)
    (moduleDir, module)
  }

  private def buildModule
      (project: Project,
       moduleModel: ModifiableModuleModel,
       moduleDir: String,
       moduleName: String,
       optCabalQuery: Option[CabalQuery])
      : Module = {
    val moduleFilePath = FileUtil.join(moduleDir, s"$moduleName.iml")
    // TODO: Parse the cabal file to determine the appropriate module name, source/test dirs, etc.
    val moduleBuilder = HaskellModuleType.getInstance.createModuleBuilder()
    moduleBuilder.setModuleFilePath(moduleFilePath)
    moduleBuilder.setContentEntryPath(moduleDir)
    moduleBuilder.setName(moduleName)
    optCabalQuery match {
      // If the cabal file was found and was parsed, query it for source roots.
      case Some(q) => setupSourceRoots(q, moduleBuilder, moduleDir)
      // Otherwise, explicitly set an empty set of source paths (to avoid the Java default 'src/')
      case None => moduleBuilder.setSourcePaths(util.Collections.emptyList())
    }
    val module = moduleBuilder.createModule(moduleModel)
    moduleBuilder.commit(project)
    module
  }

  private def setupSourceRoots
      (q: CabalQuery, moduleBuilder: HaskellModuleBuilder, moduleDir: String)
      : Unit = {
    q.getSourceRoots.foreach { dir =>
      moduleBuilder.addSourcePath(Pair.create(FileUtil.join(moduleDir, dir), ""))
    }
    // '.addSourcePath' doesn't support test sources, so we must do this manually.
    moduleBuilder.addModuleConfigurationUpdater(new ModuleBuilder.ModuleConfigurationUpdater {
      override def update(module: Module, rootModel: ModifiableRootModel): Unit = {
        rootModel.getContentEntries.collectFirst {
          case ce if ce.getFile.getPath == moduleDir => ce
        } match {
          case Some(ce) =>
            val vFileMgr = VirtualFileManager.getInstance()
            q.getTestSourceRoots.foreach { _.foreach { dir =>
              if (!new File(s"${ce.getFile.getCanonicalPath}/$dir").mkdirs()) {
                StackProjectImportBuilder.LOG.warn(new AssertionError(
                  s"Could not create directory: ${ce.getFile.getCanonicalPath}/$dir"
                ))
              }
              Option(vFileMgr.findFileByUrl(s"${ce.getUrl}/$dir")) match {
                case Some(vDir) if vDir.isDirectory =>
                  ce.addSourceFolder(FileUtil.join(ce.getUrl, dir), /* isTestSource */ true)
                case Some(_) =>
                  StackProjectImportBuilder.LOG.warn(new AssertionError(
                    s"Path is not a directory: $dir (relative to ${ce.getFile.getPath})"
                  ))
                case None =>
                  StackProjectImportBuilder.LOG.warn(new AssertionError(
                    s"VirtualFile not found: $dir (relative to ${ce.getFile.getPath})"
                  ))
              }
            }}
          case None =>
            StackProjectImportBuilder.LOG.warn(new AssertionError(
              s"Could not find content entry for module with path: $moduleDir"
            ))
        }
      }
    })
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
