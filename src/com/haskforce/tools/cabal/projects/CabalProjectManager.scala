package com.haskforce.tools.cabal.projects

import java.io.File

import com.haskforce.system.projects.{ProjectManager, Project}
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
    * registers the new Cabal-projects
    * @param file the file pointing to the cabal-file
    * @return either the RegisterError or the project
    */
  def registerNewProject(file: File) : Either[RegisterError, Project] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    if (virtualFile == null) {
      Left(FileError(file.getCanonicalPath, file.getName, s"unable to obtain VirtualFile for file ${file.getAbsolutePath}"))
    } else {
      registerNewProject(virtualFile)
    }
  }

  /**
    * registers the new Cabal-projects
    * @param file the VirtualFile pointing to the cabal-file
    * @return either the RegisterError or the project
    */
  def registerNewProject(file : VirtualFile) : Either[RegisterError, Project] = {
    val defaultProject: IProject = IProjectManager.getInstance().getDefaultProject
    PsiManager.getInstance(defaultProject).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalProject: CabalProject = new CabalProject(psiFile)
        val added: Boolean = ProjectManager.addProject(cabalProject)
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

sealed trait RegisterError
case class FileError(location: String, fileName : String, errorMsg: String) extends RegisterError
case class AlreadyRegistered(project: Project) extends RegisterError

