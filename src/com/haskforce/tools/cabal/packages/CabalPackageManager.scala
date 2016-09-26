package com.haskforce.tools.cabal.packages

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets

import com.haskforce.system.packages._
import com.haskforce.tools.cabal.CabalLanguage
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.{PsiFileFactory, PsiManager}

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
  def registerNewPackage(file: File, project: Project): Either[RegisterError, HPackage] = {
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
  def registerNewPackage(file : VirtualFile, project: Project): Either[RegisterError, HPackage] = {
    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
    getCabalFile(file, project) match {
      case Some(psiFile) => {
        val cabalPackage: CabalPackage = new CabalPackage(psiFile, file)
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
  def replaceAndRegisterNewPackage(file : VirtualFile, project: Project): Either[FileError, HPackage] = {
    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
    getCabalFile(file, project) match {
      case Some(psiFile) => {
        val cabalPackage: CabalPackage = new CabalPackage(psiFile, file)
        packageManager.replacePackage(cabalPackage)
        Right(cabalPackage)
      }
      case other =>
        Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
    }
  }

  /**
    * returns the (optional) Cabal-File for the VirtualFile
    */
  def getCabalFile(virtualFile: VirtualFile, project: Project): Option[CabalFile] = {
    if (project.isInitialized) {
      PsiManager.getInstance(project).findFile(virtualFile) match {
        case cabalFile: CabalFile => Some(cabalFile)
        case other => None
      }
    } else {
      val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
      val defaultProject: Project = ProjectManager.getInstance().getDefaultProject
      if (virtualFile.getExtension != "cabal") {
        return None
      }
      val text = try {
        new String(virtualFile.contentsToByteArray(), StandardCharsets.UTF_8)
      } catch {
        case e: IOException =>
          LOG.warn(s"Could not read CabalFile $virtualFile: $e", e)
          return None
      }
      PsiFileFactory.getInstance(defaultProject).createFileFromText(
        virtualFile.getName, CabalLanguage.INSTANCE, text
      ) match {
        case psiFile: CabalFile => Some(psiFile)
        case other =>
          LOG.warn(new AssertionError(s"Expected CabalFile, got: ${other.getClass}"))
          None
      }
    }
  }
}
