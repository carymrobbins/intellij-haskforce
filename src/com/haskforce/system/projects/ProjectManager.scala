package com.haskforce.system.projects

import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.{Project => IProject}
import com.intellij.openapi.vfs.VirtualFile

/**
  * this class retrieve, add and remove the active projects
  */
trait ProjectManager extends ProjectComponent {
  /**
    * returns the active Projects
    */
  def getProjects : Iterable[Project]

  /**
    * the main Project (used for ghci etc.)
    * @return the main Project or empty if not configured
    */
  def getMainProject : Option[Project]

  /**
    * sets the main Project
    */
  def setMainProject(project: Project)

  /**
    * sets the main Project and adds the project to the Set, replacing if an already registered is found
    */
  def replaceMainProject(packageManager: PackageManager, file: String) : Either[FileError, Project]

  /**
    * returns the Default GHC-Version
    */
  def getDefaultGHCVersion(project: IProject): Either[ExecUtil.ExecError, GHCVersion]

  /**
    * adds the project to the Set, given that there is no other project registered with the same Location
    * @param project the project to add
    * @return true if added, false if not
    */
  def addProject(project : Project) : Boolean

  /**
    * adds the project to the Set, replacing if an already registered is found
    * @param project the project to add
    * @return true if replaced, false if not
    */
  def replaceProject(project : Project) : Boolean

  /**
    * removes the project from the Set
    * @param project the project to remove
    * @return true if removed, false if not
    */
  def removeProject (project : Project) : Boolean

  /**
    * returns the Associated Project for the file
    * @param file the File to query for
    * @return the Project if found
    */
  def getProjectForFile(file: VirtualFile): Option[Project]
}
