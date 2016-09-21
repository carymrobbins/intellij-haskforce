package com.haskforce.system.projects

/**
  * this class retrieve, add and remove the active projects
  */
object ProjectManager {
  private var projects : Set[Project] = Set()

  /**
    * returns the active Projects
    */
  def getProjects : Set[Project] = projects

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
