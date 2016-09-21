package com.haskforce.system.projects

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
  def getProjects : Set[Project]

  /**
    * the main Project (used for ghci etc.)
    * @return the main Project or empty if not configured
    */
  def getMainProject : Option[Project]

  /**
    * sets the main Project and adds it to the projects if not already registered
    */
  def setMainProject(project: Project)

  /**
    * returns the Default GHC-Version
    */
  def getDefaultGHCVersion(project: IProject): Either[ExecUtil.ExecError, GHCVersion]

  /**
    * adds the project to the Set
    * @param project the project to add
    * @return true if added, false if not
    */
  def addProject(project : Project) : Boolean

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
