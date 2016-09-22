package com.haskforce.system.projects

import java.io.File

import com.haskforce.system.projects.PackageManager.{Cabal, Stack}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.{ExecUtil, NotificationUtil}
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.tools.cabal.projects.CabalProjectManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.project.{Project => IProject}
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}

/**
  * this class retrieve, add and remove the active projects
  */
class ProjectManagerImpl(intellijProject: IProject) extends ProjectManager {
  private var projects : Map[VirtualFile, Project] = Map()
  private var mainProject : Project = _

  /**
    * returns the active Projects
    */
  def getProjects : Iterable[Project] = projects.values

  /**
    * returns the Project at the location
    */
  override def getProject(file: VirtualFile): Option[Project] = projects.get(file)

  /**
    * the main Project (used for ghci etc.)
    * @return the main Project or empty if not configured
    */
  def getMainProject : Option[Project] = Option(mainProject)

  /**
    * sets the main Project
    */
  override def setMainProject(project: Project) = {
    mainProject = project
  }

  /**
    * sets the main Project and adds it to the projects if not already registered
    */
  override def replaceMainProject(packageManager: PackageManager, file: String): Either[FileError, (Option[Project], Project)] = {
    val set = (project: Project) => {
      val replaced: Option[Project] = replaceProject(project)
      setMainProject(project)
      Right((replaced, project))
    }
    val handleFunc = (registerResult: Either[RegisterError, Project]) => {
      if (registerResult.isRight) {
        set(registerResult.right.get)
      } else {
        registerResult.left.get match {
          case AlreadyRegistered(project) => set(project)
          case FileError(location, fileName, errorMsg) => Left(FileError(location, fileName, errorMsg))
        }
      }
    }
    packageManager match {
      case Cabal => {
        val registerResult: Either[RegisterError, Project] = CabalProjectManager.registerNewProject(new File(file), intellijProject)
        handleFunc(registerResult)
      }
      case Stack => {
        ??? //TODO: impl for stack
      }
    }
  }

  /**
    * returns the Default GHC-Version
    */
  def getDefaultGHCVersion(project: IProject): Either[ExecUtil.ExecError, GHCVersion] = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(project)
    val path: Either[ExecUtil.ExecError, String] = settings.getGhcPath match {
      case null => Left(new ExecError("No GHC-path configured", null))
      case "" => Left(new ExecError("GHC path is empty", null))
      case x => Right(x)
    }

    path
      .right.flatMap(path => GHCVersion.getGHCVersion(null, path))
  }

  /**
    * adds the project to the Set
    * @param project the project to add
    * @return true if added, false if not
    */
  def addProject(project : Project) : Boolean = {
    if (project.getLocation == null || project.getLocation.isDirectory) {
      return false
    }
    this.synchronized {
      if (projects.contains(project.getLocation)) {
        return false
      } else {
        projects = projects + ((project.getLocation, project))
        return true
      }
    }
  }

  /**
    * adds the project to the Set, replacing if an already registered is found
    *
    * @param project the project to add
    * @return true if replaced, false if not
    */
  override def replaceProject(project: Project): Option[Project] = {
    if (project.getLocation == null || project.getLocation.isDirectory) {
      return None
    }
    this.synchronized {
      val toReplace = projects.get(project.getLocation)
      projects = projects + ((project.getLocation, project))
      toReplace match {
        case Some(existing) => existing.emitEvent(Replace(project))
      }
      toReplace
    }
  }

  /**
    * removes the project from the Set
    * @param project the project to remove
    * @return true if removed, false if not
    */
  def removeProject (project : Project) : Boolean = {
    this.synchronized {
      if (!projects.contains(project.getLocation)) {
        return false
      } else {
        projects = projects - project.getLocation
        project.emitEvent(Remove())
        return true
      }
    }
  }

  /**
    * returns the Associated Project for the file
    * @param file the File to query for
    * @return the Project if found
    */
  def getProjectForFile(file: VirtualFile): Option[Project] = {
    projects.toList.map(tuple => (tuple._1.getParent, tuple._2))
      .filter(tuple => VfsUtilCore.isAncestor(tuple._1, file, false))
      .reduceOption((tuple1, tuple2) => {
        if (VfsUtilCore.isAncestor(tuple1._1, tuple2._1, false)) {
          tuple2
        } else {
          tuple1
        }
      })
      .map(tuple => tuple._2)
  }

  override def projectOpened(): Unit = {}

  override def projectClosed(): Unit = {}

  override def initComponent(): Unit = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(intellijProject)
    val printError = (msg : String) => {
      NotificationUtil.displaySimpleNotification(NotificationType.ERROR,
        intellijProject,
        "Unable to initialized Haskforce properly, please reset the compiler settings",
        msg)
    }
    if (settings.isCabalEnabled) {
      val result = replaceMainProject(Cabal, settings.getCabalPath)
      if (result.isLeft) {
        printError(result.left.get.errorMsg)
        settings.setUseCabal(false)
      }
    } else if (settings.isStackEnabled) {
      //TODO impl stack
    }
  }

  override def disposeComponent(): Unit = {}

  override def getComponentName: String = ProjectManagerImpl.NAME
}

object ProjectManagerImpl {
  val NAME = "haskforce.projectManager"
}

sealed trait RegisterError
case class FileError(location: String, fileName : String, errorMsg: String) extends RegisterError
case class AlreadyRegistered(project: Project) extends RegisterError
