package com.haskforce.system.projects

/**
  * this class retrieve, add and remove the active projects
  */
object ProjectManager {
  private var projects : Set[Project] = Set()
  private var mainProject : Project = null

  /**
    * returns the active Projects
    */
  def getProjects : Set[Project] = projects

  /**
    * the main Project (used for ghci etc.)
    * @return the main Project or empty if not configured
    */
  def getMainProject : Option[Project] = Option(mainProject)

  /**
    * sets the main Project and adds it to the projects if not already registered
    */
  def setMainProject(project: Project) = {
    addProject(project)
    mainProject = project
  }

  /**
    * adds the project to the Set
    * @param project the project to add
    * @return true if added, false if not
    */
  def addProject(project : Project) : Boolean = {
    if (project.getLocation == null) {
      return false
    }
    this.synchronized {
      if (projects.contains(project)) {
        return false
      } else {
        projects = projects + project
        return true
      }
    }
  }

  /**
    * removes the project from the Set
    * @param project the project to remove
    * @return true if removed, false if not
    */
  def removeProject (project : Project) : Boolean = {
    this.synchronized {
      if (!projects.contains(project)) {
        return false
      } else {
        projects = projects - project
        return true
      }
    }
  }
}
