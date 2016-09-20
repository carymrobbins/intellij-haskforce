package com.haskforce.tools.cabal.projects

import java.io.File

import com.haskforce.system.projects.{Project, ProjectManager}
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.{Project => IProject, ProjectManager => IProjectManager}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.PsiManager

/**
  * utility class used to create/delete Cabal projects
  */
object CabalProjectManager {

  private val LOG = Logger.getInstance(CabalProjectManager.getClass)

  /**
    * registers the new Cabal-Project
    * @param file the file pointing to the cabal-file
    * @return Some if the cabal-file is found and not already registered
    */
  def registerNewProject(file: File) : Option[Project] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    registerNewProject(virtualFile)
  }

  /**
    * registers the new Cabal-Project
    * @param file the VirtualFile pointing to the cabal-file
    * @return Some if the cabal-file is found and not already registered
    */
  def registerNewProject(file : VirtualFile) : Option[Project] = {
    val defaultProject: IProject = IProjectManager.getInstance().getDefaultProject
    PsiManager.getInstance(defaultProject).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalProject: CabalProject = new CabalProject(psiFile)
        val added: Boolean = ProjectManager.addProject(cabalProject)
        if (added) {
          Some(cabalProject)
        } else {
          LOG.warn(new AssertionError(s"Project for file $file is already registered"))
          None
        }
      }
      case other =>
        LOG.warn(new AssertionError(s"Expected CabalFile, got: ${other.getClass}"))
        None
    }
  }
}
