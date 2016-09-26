package com.haskforce.system.packages

import java.io.File
import java.util

import com.haskforce.haskell.{HaskellModuleBuilder, HaskellModuleType}
import com.haskforce.system.packages.BuildType.{Benchmark, Executable, Library, TestSuite}
import com.intellij.ide.util.projectWizard.ModuleBuilder
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{ModifiableModuleModel, Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile, VirtualFileManager}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
  * sets up an Intellij-Project based on the HPackages
  */
object ProjectSetup {
  private val LOG = Logger.getInstance(ProjectSetup.getClass)

  /**
    * Sets up an Intellij Project. Must be run within a write action, also the Project must be initialized.q^^
    * @param packages the packages
    * @param project the active project
    * @param update whether its an creation or an update
    * @return a List of created Modules
    */
  def setUp(packages: List[HPackage], project: Project,
            moduleModel: ModifiableModuleModel, projectRoot: String, update: Boolean) : List[Module] = {
    var createdModules: List[Module] = createMissingModules(packages, moduleModel, project)
    // If we aren't updating an existing project AND the project root isn't a module,
    // let's create it as the "root" module.
    val existingModules: Set[File] = HaskellModuleType.findModules(project).asScala
      .flatMap(m => Option(m.getModuleFile))
      .flatMap(file => Option(file.getParent))
      .filter(_.isInLocalFileSystem)
      .map(file => new File(file.getCanonicalPath))
      .toSet
    if (!update && !existingModules.contains(new File(projectRoot))) {
      val projectName = new File(projectRoot).getName
      val rootPackage: Option[HPackage] = packages.find(pkg =>
        pkg.getLocation.isInLocalFileSystem
          && new File(pkg.getLocation.getParent.getCanonicalPath).equals(new File(projectRoot)))
      val projectModule = rootPackage match {
        case Some(pkg) => createMissingModule(pkg, moduleModel, project)
        case None => createDummyModule(project, moduleModel, projectRoot, projectName + " (root)")
      }
      createdModules = projectModule +: createdModules
    }
    createdModules
  }

  /**
    * creates the missing Intellij-Modules (also creates missing src/test etc directories, so it must be run within a write-action)
    */
  def createMissingModules(packages: List[HPackage],
                           moduleModel: ModifiableModuleModel, project: Project): List[Module] = {
    createMissingModules(packages, moduleModel, project.getBaseDir.getCanonicalPath, project)
  }

  /**
    * creates the missing Intellij-Modules (also creates missing src/test etc directories, so it must be run within a write-action)
    */
  private def createMissingModules(packages: List[HPackage], moduleModel: ModifiableModuleModel, projectRoot: String, project: Project): List[Module] = {
    val modules: Iterable[VirtualFile] = HaskellModuleType.findModules(project).asScala
      .flatMap(m => Option(m.getModuleFile))
      .flatMap(file => Option(file.getParent))

    //we don't need the packages that are inside a module or the BaseDir
    val newPackages: List[HPackage] = packages
      .filter(pkg => {
        !(pkg.getLocation.isInLocalFileSystem && new File(pkg.getLocation.getParent.getCanonicalPath).equals(new File(projectRoot)))
      })
      .filter(pkg => !modules.exists(existing => {
        VfsUtilCore.isAncestor(existing, pkg.getLocation.getParent, true) || VfsUtilCore.isAncestor(pkg.getLocation.getParent, existing, true)
      }))

    newPackages.map(pkg => createMissingModule(pkg, moduleModel, project))
  }

  /**
    * creates a Module for the package
    */
  private def createMissingModule(hPackage: HPackage, moduleModel: ModifiableModuleModel, project: Project) : Module = {
    val name: String = hPackage.getLocation.getNameWithoutExtension
    val moduleDir: String = hPackage.getLocation.getParent.getPath
    val moduleName = determineProperModuleName(project, name)
    val moduleBuilder = HaskellModuleType.getInstance.createModuleBuilder()
    moduleBuilder.setModuleFilePath(FileUtil.join(moduleDir, moduleName + ".iml"))
    moduleBuilder.setContentEntryPath(moduleDir)
    moduleBuilder.setName(moduleName)
    markDirectories(hPackage, moduleBuilder)
    val module = moduleBuilder.createModule(moduleModel)
    moduleBuilder.commit(project)
    module
  }

  private def createDummyModule(project: Project, moduleModel: ModifiableModuleModel, moduleDir: String, moduleName: String): Module = {
    val moduleFilePath = FileUtil.join(moduleDir, s"$moduleName.iml")
    val moduleBuilder = HaskellModuleType.getInstance.createModuleBuilder()
    moduleBuilder.setModuleFilePath(moduleFilePath)
    moduleBuilder.setContentEntryPath(moduleDir)
    moduleBuilder.setName(moduleName)
    moduleBuilder.setSourcePaths(util.Collections.emptyList())
    val module = moduleBuilder.createModule(moduleModel)
    moduleBuilder.commit(project)
    module
  }
  /**
    * sets the source-directories etc. for the package
    */
  private def markDirectories(hPackage: HPackage, moduleBuilder: HaskellModuleBuilder): Unit = {
    val moduleDir: String = hPackage.getLocation.getParent.getPath
    val sourcePaths: Set[String] = hPackage.getBuildInfo
      .toSet
      .filter(info => info.typ match {
        case Library => true
        case Executable => true
        case other => false
      })
      .flatMap(info => info.getSourceDirs)

    //to avoid the Java default 'src/'
    moduleBuilder.setSourcePaths(util.Collections.emptyList())
    if (sourcePaths.nonEmpty) {
      sourcePaths.foreach { dir =>
        moduleBuilder.addSourcePath(Pair.create(FileUtil.join(moduleDir, dir), ""))
      }
    }
    // '.addSourcePath' doesn't support test sources, so we must do this manually.
    moduleBuilder.addModuleConfigurationUpdater(new ModuleBuilder.ModuleConfigurationUpdater {
      override def update(module: Module, rootModel: ModifiableRootModel): Unit = {
        rootModel.getContentEntries.collectFirst {
          case ce if ce.getFile.getPath == moduleDir => ce
        } match {
          case Some(ce) =>
            val vFileMgr = VirtualFileManager.getInstance()
            //setup TestDirs
            val testDirs: Set[String] = hPackage.getBuildInfo
              .toSet
              .filter(info => info.typ match {
                case TestSuite => true
                case Benchmark => true
                case other => false
              })
              .flatMap(info => info.getSourceDirs)
            if (testDirs.nonEmpty) {
              testDirs.foreach(dir => {
                val file: File = new File(s"${ce.getFile.getCanonicalPath}/$dir")
                if (!file.exists() && !file.mkdirs()) {
                  LOG.warn(new AssertionError(
                    s"Could not create directory: ${ce.getFile.getCanonicalPath}/$dir"
                  ))
                }
                Option(vFileMgr.findFileByUrl(s"${ce.getUrl}/$dir")) match {
                  case Some(vDir) if vDir.isDirectory =>
                    ce.addSourceFolder(FileUtil.join(ce.getUrl, dir), /* isTestSource */ true)
                  case Some(_) =>
                    LOG.warn(new AssertionError(
                      s"Path is not a directory: $dir (relative to ${ce.getFile.getPath})"
                    ))
                  case None =>
                    LOG.warn(new AssertionError(
                      s"VirtualFile not found: $dir (relative to ${ce.getFile.getPath})"
                    ))
                }
              })
            }
          case None =>
            LOG.warn(new AssertionError(
              s"Could not find content entry for module with path: $moduleDir"
            ))
        }
      }
    })
  }

  /**
    * Determines a proper module name that doesn't clash with existing modules.
    */
  private def determineProperModuleName(project: Project, name: String): String = {
    val moduleNames = ModuleManager.getInstance(project).getModules.map(_.getName.toLowerCase)
    @tailrec
    def loop(suffix: Int): String = {
      // Append "-suffix" to name if there are conflicts.
      val newName = name + (if (suffix == 0) "" else "-" + suffix)
      if (moduleNames.contains(newName)) loop(suffix + 1)
      else newName
    }
    loop(0)
  }
}
