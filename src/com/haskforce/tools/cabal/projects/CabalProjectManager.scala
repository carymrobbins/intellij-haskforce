package com.haskforce.tools.cabal.projects

import java.io.File

import com.haskforce.system.projects._
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.{Project => IProject}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.PsiManager

/**
  * utility class used to create/delete Cabal projects
  */
object CabalProjectManager {

  private val LOG = Logger.getInstance(CabalProjectManager.getClass)

  /**
    * registers the new Cabal-projects
    * @param file the file pointing to the cabal-file
    * @param project the Intellij Project
    * @return either the RegisterError or the project
    */
  def registerNewProject(file: File, project: IProject) : Either[RegisterError, Project] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    if (virtualFile == null) {
      Left(FileError(file.getCanonicalPath, file.getName, s"unable to obtain VirtualFile for file ${file.getAbsolutePath}"))
    } else {
      registerNewProject(virtualFile, project)
    }
  }

  /**
    * registers the new Cabal-projects
    * @param file the VirtualFile pointing to the cabal-file
    * @param project the Intellij Project
    * @return either the RegisterError or the project
    */
  def registerNewProject(file : VirtualFile, project: IProject) : Either[RegisterError, Project] = {
    val projectManager: ProjectManager = project.getComponent(classOf[ProjectManager])
    PsiManager.getInstance(project).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalProject: CabalProject = new CabalProject(psiFile)
        val added: Boolean = projectManager.addProject(cabalProject)
        if (added) {
          Right(cabalProject)
        } else {
          Left(AlreadyRegistered(cabalProject))
        }
      }
      case other =>
        Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
    }
  }
}

