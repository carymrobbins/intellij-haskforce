package com.haskforce.system.packages

import java.io.File
import java.util

import com.haskforce.haskell.{HaskellModuleBuilder, HaskellModuleType}
import com.haskforce.system.packages.BuildType.{Benchmark, Executable, Library, TestSuite}
import com.intellij.ide.util.projectWizard.ModuleBuilder
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{ModifiableModuleModel, Module, ModuleManager, ModuleServiceManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ContentEntry, ModifiableRootModel, ModuleRootManager, ModuleRootModificationUtil}
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.{Computable, Pair}
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile, VirtualFileManager}
import com.intellij.util.Consumer

import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
  * sets up an Intellij-Project based on the HPackages
  */
//TODO set service even if module is missing!
object ProjectSetup {
  private val LOG = Logger.getInstance(ProjectSetup.getClass)

  /**
    * Sets up an Intellij Project. Must be run within a write action.
    * @param packages the packages
    * @param project the active project
    * @param updateExistingModules whether its an creation or an update
    * @param setupRoot whether to create a
    * @return a List of shadowed Packages(packages that are contained, but are not equal to other packages),
    *         a List of existing Modules and a List of created Modules
    */
  //TODO changed update => setupRoot
  def setUp(packages: List[HPackage], project: Project,
            moduleModel: ModifiableModuleModel, projectRoot: String, updateExistingModules: Boolean, setupRoot: Boolean)
                                                        : (List[ExistingModule], List[ExistingModule], List[Module]) = {
    val (shadowed, existing, newPackages) = resolveModuleStatusForPackages(packages, project)

    if (setupRoot) {
      existing.foreach(existingModule => updateModule(existingModule.hPackage, existingModule.module, moduleModel, project))
    }
    var createdModules: List[Module] = newPackages.map(hPackage => createMissingModule(hPackage, moduleModel, project))
    // If we aren't updating an existing project AND the project root isn't a module,
    // let's create it as the "root" module.
    val existingModules: Set[File] = moduleModel.getModules
      .flatMap(module => ModuleRootManager.getInstance(module).getContentRoots)
      .flatMap(root => Option(root.getParent))
      .filter(_.isInLocalFileSystem)
      .map(file => new File(file.getCanonicalPath))
      .toSet
    if (setupRoot && !existingModules.contains(new File(projectRoot))) {
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
    (shadowed, existing, createdModules)
  }

  /**
    * adds a package, must be run within a write-action
    * @param hPackage the package to add
    * @param project the current project
    * @param update whether the existing should be updated
    * @return either an AddPackageError (won't return Existing if update is true) or the module
    */
  def addPackage(hPackage: HPackage, project: Project, update: Boolean): Either[AddPackageError, Module] = {
    val sourceDirs: Set[VirtualFile] = hPackage.getBuildInfo.toSet
      .flatMap(info => info.getSourceDirs)
      .flatMap(dir => com.haskforce.system.utils.FileUtil.fromRelativePath(dir, hPackage.getLocation.getParent.getPath))

    val contentRootsModuleList: Array[(VirtualFile, Module)] = ModuleManager.getInstance(project).getModules
      .flatMap(module => ModuleRootManager.getInstance(module).getContentRoots.zip(Stream.continually(module)))

    val shadowed: Option[PackageShadowed] = getShadowed(contentRootsModuleList, hPackage, sourceDirs)
      .map(existingModule => PackageShadowed(existingModule.matchingSourceDir, existingModule.module, existingModule.matchingContentRoot))
    if (shadowed.isDefined) {
      return Left(shadowed.get)
    }

    val modifiableModel: ModifiableModuleModel = ModuleManager.getInstance(project).getModifiableModel
    val matching: Option[Existing] = getExisting(contentRootsModuleList, hPackage, sourceDirs)
      .map(existingModule => Existing(existingModule.matchingSourceDir, existingModule.module))
    if (matching.isDefined) {
      if (update) {
        Right(updateModule(hPackage, matching.get.module, modifiableModel, project))
      } else {
        Left(matching.get)
      }
    } else {
      Right(createMissingModule(hPackage, modifiableModel, project))
    }
  }


  /**
    * determines whether a package is shadowed by an existing module, the module is already existing or is is new
    * @return (shadowed, existing, new)
    */
  private def resolveModuleStatusForPackages(packages: List[HPackage], project: Project): (List[ExistingModule], List[ExistingModule], List[HPackage]) = {
    val packageSourceDirList: List[(HPackage, Set[VirtualFile])] = packages.map(hPackage => {
      val sourceDirs: Set[VirtualFile] = hPackage.getBuildInfo.toSet
        .flatMap(info => info.getSourceDirs)
        .flatMap(dir => com.haskforce.system.utils.FileUtil.fromRelativePath(dir, hPackage.getLocation.getParent.getPath))
      (hPackage, sourceDirs)
    })

    val contentRootsModuleList: Array[(VirtualFile, Module)] = ModuleManager.getInstance(project).getModules
      .flatMap(module => ModuleRootManager.getInstance(module).getContentRoots.zip(Stream.continually(module)))


    //shadowed packageSourceDir, package, shadowing moduleContent, module
    val shadowed: List[ExistingModule] = packageSourceDirList.flatMap(tuple => {
      val (hPackage, locations) = tuple
      getShadowed(contentRootsModuleList, hPackage, locations)
    })

    val shadowedPackages: Set[HPackage] = shadowed.map(existingModule => existingModule.hPackage).toSet

    val existing: List[ExistingModule] = packageSourceDirList
      .filter(tuple => !shadowedPackages.contains(tuple._1))
      .flatMap(tuple => {
        val (hPackage, locations) = tuple
        getExisting(contentRootsModuleList, hPackage, locations)
      })

    val existingPackages = existing.map(existingModule => existingModule.hPackage).toSet

    val newPackages: List[HPackage] = packages
      .filter(hPackage => !shadowedPackages.contains(hPackage) && !existingPackages.contains(hPackage))
    (shadowed, existing, newPackages)
  }

  private def getExisting(contentRootsModuleList: Array[(VirtualFile, Module)], hPackage: HPackage, locations: Set[VirtualFile]): Option[ExistingModule] = {
    //a package is shadowed if at least one of the source-locations is equal to another content-root
    contentRootsModuleList.flatMap(tuple => {
      val (contentRoot, module) = tuple
      locations.find(location => contentRoot == location)
        .map(packageContentRoot => ExistingModule(packageContentRoot, hPackage, contentRoot, module))
    }).headOption
  }

  private def getShadowed(contentRootsModuleList: Array[(VirtualFile, Module)], hPackage: HPackage, locations: Set[VirtualFile]): Option[ExistingModule] = {
    //a package is shadowed if at least one of the source-locations is inside another modules content root, but does not equal the content-root
    contentRootsModuleList.flatMap(tuple => {
      val (contentRoot, module) = tuple
      locations.find(location => VfsUtilCore.isAncestor(contentRoot, location, true))
        .map(packageContentRoot => ExistingModule(packageContentRoot, hPackage, contentRoot, module))
    }).headOption
  }

  /**
    * creates a Module for the package and registers the package, must be run within an write-action
    */
  private def createMissingModule(hPackage: HPackage, moduleModel: ModifiableModuleModel, project: Project) : Module = {
    val name: String = hPackage.getName.getOrElse(hPackage.getLocation.getNameWithoutExtension)
    val moduleDir: String = hPackage.getLocation.getParent.getPath
    val moduleName = determineProperModuleName(project, name)
    val moduleBuilder = HaskellModuleType.getInstance.createModuleBuilder()
    moduleBuilder.setModuleFilePath(FileUtil.join(moduleDir, moduleName + ".iml"))
    moduleBuilder.setContentEntryPath(moduleDir)
    moduleBuilder.setName(moduleName)
    markDirectories(hPackage, moduleBuilder)
    val module = moduleBuilder.createModule(moduleModel)
    HPackageModule.getInstance(module).replacePackage(hPackage)
    moduleBuilder.commit(project)
    module
  }

  /**
    * updates the Module,
    */
  private def updateModule(newPackage: HPackage, module: Module, moduleModel: ModifiableModuleModel, project: Project): Module = {
    val packageModule: HPackageModule = ModuleServiceManager.getService(module, classOf[HPackageModule])
    val oldPackage: Option[HPackage] = packageModule.optPackage
    ModuleRootModificationUtil.updateModel(module, new Consumer[ModifiableRootModel] {
      override def consume(modifiableRootModel: ModifiableRootModel): Unit = {
        modifiableRootModel.clear()
        updateModifiableRootModel(modifiableRootModel, newPackage)
      }
    })
    if (module.getName != newPackage.getName.getOrElse(newPackage.getLocation.getNameWithoutExtension)) {
      moduleModel.renameModule(module, newPackage.getName.getOrElse(newPackage.getLocation.getNameWithoutExtension))
      moduleModel.commit()
    }
    packageModule.replacePackage(newPackage)
    oldPackage.foreach(pkg => pkg.emitEvent(Replace(newPackage)))
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
        updateModifiableRootModel(rootModel, hPackage)
      }
    })
  }

  private def updateModifiableRootModel(modifiableRootModel: ModifiableRootModel, hPackage: HPackage): Unit = {
    val moduleDir: String = hPackage.getLocation.getParent.getPath
    val vFileMgr = VirtualFileManager.getInstance()
    val markDirectories = (ce: ContentEntry, directories: Set[String], doMark: String => Unit) => {
      if (directories.nonEmpty) {
        directories.foreach(dir => {
          val file: File = new File(s"${ce.getFile.getCanonicalPath}/$dir")
          if (!file.exists() && !file.mkdirs()) {
            LOG.warn(new AssertionError(
              s"Could not create directory: ${ce.getFile.getCanonicalPath}/$dir"
            ))
          }
          Option(vFileMgr.findFileByUrl(s"${ce.getUrl}/$dir")) match {
            case Some(vDir) if vDir.isDirectory =>
              doMark(FileUtil.join(ce.getUrl, dir))
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
    }
    modifiableRootModel.getContentEntries.collectFirst {
      case ce if ce.getFile.getPath == moduleDir => ce
    } match {
      case Some(ce) =>
        //setup TestDirs
        val testDirs: Set[String] = hPackage.getBuildInfo
          .toSet
          .filter(info => info.typ match {
            case TestSuite => true
            case Benchmark => true
            case other => false
          })
          .flatMap(info => info.getSourceDirs)
        val sourceDirs: Set[String] = hPackage.getBuildInfo
          .toSet
          .filter(info => info.typ match {
            case Library => true
            case Executable => true
            case other => false
          })
          .flatMap(info => info.getSourceDirs)
        markDirectories(ce, testDirs, path => ce.addSourceFolder(path, /* isTestSource */ true))
        markDirectories(ce, sourceDirs, path => ce.addSourceFolder(path, /* isTestSource */ false))
      case None =>
        LOG.warn(new AssertionError(
          s"Could not find content entry for module with path: $moduleDir"
        ))
    }
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

case class ExistingModule(matchingSourceDir: VirtualFile, hPackage: HPackage, matchingContentRoot: VirtualFile, module: Module)

sealed trait AddPackageError
case class PackageShadowed(shadowedSourceDir: VirtualFile, module: Module, shadowingContentRoot: VirtualFile) extends AddPackageError
case class Existing(violatingSourceDir: VirtualFile, module: Module) extends AddPackageError
