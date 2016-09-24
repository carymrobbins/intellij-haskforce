package com.haskforce.tools.cabal.packages

import java.io.File
import com.haskforce.system.packages._
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.PsiManager

/**
  * utility class used to create/delete Cabal packages
  */
object CabalPackageManager extends BackingPackageManager {
  private val LOG = Logger.getInstance(CabalPackageManager.getClass)

  override def sameBacking(other: BackingPackageManager): Boolean = other == CabalPackageManager

  override def replaceMain(file : VirtualFile, old: Option[HPackage], project: Project): Either[FileError, List[FileError]] = {
    val packageManager = project.getComponent(classOf[HPackageManager])
    CabalPackageManager.replaceAndRegisterNewPackage(file, project)
      .right.map(pkg => {
      packageManager.setMainPackage(pkg)
      if (old.isDefined && old.get.getProjectInformation.isDefined) {
        old.get.getProjectInformation.get.getRelatedPackages
          .map(onePackage => CabalPackageManager.replaceAndRegisterNewPackage(onePackage.getLocation, project))
          .map(either => either.left.toOption)
          .filter(_.isDefined)
          .map(_.get)
      } else {
        Nil
      }
    })
  }

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
          val registered: Option[HPackage] = packageManager.getPackage(file)
          if (registered.isEmpty) {
            LOG.error("unable to register and getPackage returned empty!")
            Left(AlreadyRegistered(cabalPackage))
          }
          Left(AlreadyRegistered(registered.get))
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

