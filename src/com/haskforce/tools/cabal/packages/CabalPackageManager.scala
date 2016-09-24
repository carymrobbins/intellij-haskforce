package com.haskforce.tools.cabal.packages

import java.io.File

import com.haskforce.system.packages.PackageManager.Cabal
import com.haskforce.system.packages._
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.PsiManager

/**
  * utility class used to create/delete Cabal packages
  */
object CabalPackageManager {
  private val LOG = Logger.getInstance(CabalPackageManager.getClass)

  /**
    * registers the new Cabal-packages
    * @param file the file pointing to the cabal-file
    * @param project the Intellij Project
    * @return either the RegisterError or the package
    */
  def registerNewPackage(file: File, project: Project) : Either[RegisterError, HPackage] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    if (virtualFile == null) {
      Left(FileError(file.getCanonicalPath, file.getName, s"unable to obtain VirtualFile for file ${file.getAbsolutePath}"))
    } else {
      registerNewPackage(virtualFile, project)
    }
  }

  /**
    * registers the new Cabal-packages
    * @param file the VirtualFile pointing to the cabal-file
    * @param project the Intellij Project
    * @return either the RegisterError or the package
    */
  def registerNewPackage(file : VirtualFile, project: Project) : Either[RegisterError, HPackage] = {
    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
    PsiManager.getInstance(project).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalPackage: CabalPackage = new CabalPackage(psiFile)
        val added: Boolean = packageManager.addPackage(cabalPackage)
        if (added) {
          Right(cabalPackage)
        } else {
          Left(AlreadyRegistered(cabalPackage))
        }
      }
      case other =>
        Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
    }
  }

  /**
    * registers the new Cabal-packages, replaces existing if not equal
    * @param file the VirtualFile pointing to the cabal-file
    * @param project the Intellij Project
    * @return either the RegisterError or the package
    */
  def replaceAndRegisterNewPackage(file : VirtualFile, project: Project) : Either[FileError, HPackage] = {
    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
    PsiManager.getInstance(project).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalPackage: CabalPackage = new CabalPackage(psiFile)
        packageManager.replacePackage(cabalPackage)
        Right(cabalPackage)
      }
      case other =>
        Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
    }
  }
}

