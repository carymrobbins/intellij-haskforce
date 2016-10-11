package com.haskforce.system.settings

import com.haskforce.jps.model.HaskellBuildOptions
import com.haskforce.system.packages._
import com.haskforce.system.utils.NotificationUtil
import com.haskforce.tools.cabal.actions.DiscoverCabalPackagesAction
import com.haskforce.tools.stack.packages.StackPackageManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.StartupActivity
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}

class SettingsMigrationActivity extends StartupActivity {
  override def runActivity(project: Project): Unit = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(project)
    val oldVersion: Int = settings.getSettingsVersion
    if (oldVersion == 0) {
      if (settings.isStackEnabled) {
        val stackYamlPath: String = settings.getStackFile
        val path: VirtualFile = LocalFileSystem.getInstance().findFileByPath(stackYamlPath)
        if (path == null) {
          NotificationUtil.displaySimpleNotification(
            NotificationType.ERROR, project,
            "unable to perform automatic haskforce settings version migration",
            s"unable to obtain VirtualFile for $stackYamlPath, please re-import the project"
          )
          return
        }
        val packagesEither = StackPackageManager.getPackages(path, project)
        val success: Boolean = addAndNotifyUser(project, s"stackfile: $stackYamlPath", packagesEither)
        if (success) {
          settings.setSettingsVersion(HaskellBuildOptions.currentVersion)
        }
      } else if (settings.isCabalEnabled) {
        NotificationUtil.displaySimpleNotification(
          NotificationType.WARNING, project,
          "Haskforce Version migration",
          s"if there are undiscovered cabal packages, please run Tools -> ${DiscoverCabalPackagesAction.TITLE} for full support in newer haskforce versions"
        )
        settings.setSettingsVersion(HaskellBuildOptions.currentVersion)
      }
    }
  }

  def addAndNotifyUser(project: Project, packageSource: String, packagesEither: Either[FileError, List[Either[FileError, HPackage]]]): Boolean = {
    if (packagesEither.isLeft) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, project,
        "unable to perform automatic haskforce settings version migration",
        s"unable to retrieve packages from s$packageSource, please re-import the project"
      )
      return false
    }
    val errorOrPackage: List[Either[FileError, HPackage]] = packagesEither.right.get
    val errors: List[FileError] = errorOrPackage.flatMap(_.left.toOption)
    if (errors.nonEmpty) {
      val errorsMessages: String = errors.map(error => s"Error in file ${error.location}: ${error.errorMsg}").mkString("<br/>")
      NotificationUtil.displaySimpleNotification(
        NotificationType.WARNING, project,
        "errors occurred while performing automatic haskforce settings version migration",
        s"unable to retrieve all packages from $packageSource<br/>$errorsMessages"
      )
    }
    val packages: Set[HPackage] = errorOrPackage.flatMap(_.right.toOption).toSet
    if (packages.isEmpty) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, project,
        "errors occurred while performing automatic haskforce settings version migration",
        s"unable to retrieve any package from $packageSource, please re-import the project"
      )
      return false
    }
    val alreadyInit: Set[HPackage] = packages.flatMap(pkg => ProjectSetup.getExisting(pkg, project).map(ex => (ex, pkg)))
      .filter(tuple => HPackageModule.getInstance(tuple._1.module).optPackage.isDefined)
      .map(tuple => tuple._2)
    val changed: Set[(Either[AddPackageError, ChangedModule], HPackage)] = packages.diff(alreadyInit)
      .map(hPackage => (ProjectSetup.addPackage(hPackage, project, update = true), hPackage))
    val addPackageErrors = changed.flatMap(tuple => tuple._1.left.toOption.map((_, tuple._2)))
    if (addPackageErrors.nonEmpty) {
      val errorMessages: String = addPackageErrors.map(tuple => s"unable to create module for package ${tuple._2.getName.getOrElse(tuple._2.getLocation.getNameWithoutExtension)}, "
        + (tuple._1 match {
        case PackageShadowed(shadowedDir, m, shadowedContentRoot) => s"the packages contentRoot $shadowedContentRoot is already registered by module ${m.getName}"
        case Existing(m) => s"a module is already existing under the name ${m.getName}, but haskforce was unable to update it"
      })).mkString("<br/>")
      NotificationUtil.displaySimpleNotification(
        NotificationType.ERROR, project,
        "errors occurred while performing automatic haskforce settings version migration",
        s"haskforce was not able to fully initialize the modules, a re-import might be needed.<br>" + errorMessages
      )
    }
    val affected: Set[(ChangedModule, HPackage)] = changed.flatMap(tuple => tuple._1.right.toOption.map((_, tuple._2)))
    NotificationUtil.displaySimpleNotification(
      NotificationType.INFORMATION, project,
      "haskforce settings version migration succeeded",
      "haskforce updated the following modules:<br/>"
        + affected.map(tuple => s"for package ${tuple._2.getName.getOrElse(tuple._2.getLocation.getNameWithoutExtension)} added or updated module ${tuple._1.module.getName}")
        .mkString("<br/>")
    )
    true
  }
}
