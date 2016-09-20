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
    */
  def addProject (project : Project) : Unit = {
    this.synchronized {
      projects = projects + project
    }
  }

  /**
    * removes the Project from the Set
    * @param project the project to remove
    */
  def removeProject (project : Project) : Unit = {
    this.synchronized {
      projects = projects - project
    }
  }
}
