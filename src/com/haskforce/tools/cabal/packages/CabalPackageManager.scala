package com.haskforce.tools.cabal.packages

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets

import com.haskforce.system.packages._
import com.haskforce.system.utils.FileUtil
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
  private val NAME = "haskforce.CabalPackageManager"

  override def sameBacking(other: BackingPackageManager): Boolean = other == CabalPackageManager

  override def getName: String = NAME

  override def getPackages(file : VirtualFile, project: Project): Either[FileError, List[Either[FileError, HPackage]]] = {
    getCabalFile(file, project)
      .right.map(cabalFile => List(Right(new CabalPackage(cabalFile, file))))
  }

  override def getPackageFromState(hPackageState: HPackageState, packageLocation: VirtualFile, project: Project): Option[HPackage] = {
    if (hPackageState.getPackageManager == NAME) {
      try {
        getCabalFile(packageLocation, project).right.toOption.map(cabalFile => new CabalPackage(cabalFile, packageLocation))
      } catch {
        case exception: ClassCastException => LOG.error(s"hPackageState is not from type CabalPackageState", exception)
          None
      }
    } else {
      LOG.error(s"received hPackageState from PackageManager ${hPackageState.getPackageManager}")
      None
    }
  }

  /**
    * registers the new Cabal-packages
    * @param file the file pointing to the cabal-file
    * @param project the Intellij Project
    * @param replace true to replace an already existing package
    * @return either the RegisterError or the package
    */
  def registerNewPackage(file: File, project: Project, replace: Boolean): Either[RegisterError, HPackage] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    if (virtualFile == null) {
      Left(FileError(file.getCanonicalPath, file.getName, s"unable to obtain VirtualFile for file ${file.getAbsolutePath}"))
    } else {
      registerNewPackage(virtualFile, project, replace)
    }
  }

  /**
    * registers the new Cabal-packages
    * @param file the VirtualFile pointing to the cabal-file
    * @param project the Intellij Project
    * @param update true to update an already existing package
    * @return either the RegisterError or the package
    */
  def registerNewPackage(file : VirtualFile, project: Project, update: Boolean): Either[RegisterError, HPackage] = {
    getCabalFile(file, project).right.flatMap[RegisterError, CabalPackage](psiFile => {
      val cabalPackage: CabalPackage = new CabalPackage(psiFile, file)
      ProjectSetup.addPackage(cabalPackage, project, update)
        .right.map(_ => cabalPackage)
        .left.map {
        case PackageShadowed(shadowedSourceDir, module, shadowingContentRoot) => ShadowedByRegistered(shadowedSourceDir, module, shadowingContentRoot, cabalPackage)
        case Existing(module) => AlreadyRegistered(module, cabalPackage)
      }
    })
  }

  /**
    * returns the (optional) Cabal-File for the VirtualFile
    */
  def getCabalFile(file: VirtualFile, project: Project): Either[FileError, CabalFile] = {
    if (project.isInitialized) {
      PsiManager.getInstance(project).findFile(file) match {
        case cabalFile: CabalFile => Right(cabalFile)
        case other => Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
      }
    } else {
      val defaultProject: Project = ProjectManager.getInstance().getDefaultProject
      if (file.getExtension != "cabal") {
        return Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${file.getExtension}"))
      }
      val text = try {
        new String(file.contentsToByteArray(), StandardCharsets.UTF_8)
      } catch {
        case e: IOException =>
          LOG.warn(s"Could not read CabalFile $file: $e", e)
          return Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Could not read CabalFile $file: $e"))
      }
      PsiFileFactory.getInstance(defaultProject).createFileFromText(
        file.getName, CabalLanguage.INSTANCE, text
      ) match {
        case psiFile: CabalFile => Right(psiFile)
        case other =>
          LOG.warn(new AssertionError(s"Expected CabalFile, got: ${other.getClass}"))
          Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
      }
    }
  }

}
