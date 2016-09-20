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
    * adds the Project to the Set
    * @param project the project to add
    * @return true if added, false if not
    */
  def addProject (project : Project) : Boolean = {
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
    * removes the Project from the Set
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
