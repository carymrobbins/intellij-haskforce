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
    * returns the Project at the location
    */
  def getProject(file: VirtualFile) : Option[Project]

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
    * @return an Error, or an tuple with the optional replaced project and the created one
    */
  def replaceMainProject(packageManager: PackageManager, file: String) : Either[FileError, (Option[Project], Project)]

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
    * @return the old project if replace, or Empty
    */
  def replaceProject(project : Project) : Option[Project]

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
