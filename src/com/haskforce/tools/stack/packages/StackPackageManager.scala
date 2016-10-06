package com.haskforce.tools.stack.packages

import java.io.File

import com.haskforce.importWizard.stack.StackYaml
import com.haskforce.system.packages._
import com.haskforce.system.utils.FileUtil
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scalaz.\/

/**
  * utility class used to create/delete Stack packages
  */
object StackPackageManager extends BackingPackageManager {
  private val LOG = Logger.getInstance(StackPackageManager.getClass)
  private val NAME = "haskforce.StackPackageManager"

  override def sameBacking(other: BackingPackageManager): Boolean = other == StackPackageManager

//  override def replaceMain(file: VirtualFile, old: Option[HPackage], project: Project): Either[FileError, List[FileError]] = {
//    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
//    StackPackageManager.getPackages(file, project)
//      .right.flatMap(list => {
//      val packages: List[Either[FileError, (StackPackage, Option[HPackage])]] = list.map(either => {
//        either.right.map(stackPackage => {
//          (stackPackage, StackPackageManager.replacePackage(stackPackage, project))
//        })
//      })
//
//      packages.toStream
//        .map(_.right.toOption)
//        .filter(_.isDefined)
//        .map(_.get)
//        .map(_._1)
//        .headOption match {
//        case None => Left(FileError(
//          file.getCanonicalPath,
//          file.getNameWithoutExtension,
//          s"unable to retrieve any packages from the stackFile ${file.getCanonicalPath}")
//        )
//        case Some(pkg) => {
//          packageManager.setMainPackage(pkg)
//          val errors: List[FileError] = packages.map(_.left.toOption)
//            .filter(_.isDefined)
//            .map(_.get)
//          Right(errors)
//        }
//      }
//    })
//  }
//
//  /**
//    * returns the
//    * @param stack the file pointing to the stack-file
//    * @param project the Intellij Project
//    * @return either a FileError if the stack-file is not found/illegal or a list of StackPackages (or errors if they are not found)
//    */
//  def getPackages(stack: File, project: Project): Either[FileError, List[Either[FileError, StackPackage]]] = {
//    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(stack)
//    if (virtualFile == null) {
//      Left(FileError(stack.getCanonicalPath, stack.getName, s"unable to obtain VirtualFile for file ${stack.getAbsolutePath}"))
//    } else {
//      getPackages(virtualFile, project)
//    }
//  }
//
//  /**
//    * registers the new Cabal-projects
//    * @param stackFile the VirtualFile pointing to the stack-file
//    * @param project the Intellij Project
//    * @return either a FileError if the stack-file is not found/illegal or a list of StackPackages (or errors if they are not found)
//    */
//  def getPackages(stackFile : VirtualFile, project: Project): Either[FileError, List[Either[FileError, StackPackage]]] = {
//    val parent: VirtualFile = stackFile.getParent
//    if (parent == null)
//      Left(FileError(stackFile.getCanonicalPath, stackFile.getNameWithoutExtension, s"stack file is root directory"))
//    val buildDir = Option(parent.getChildren).flatMap(_.find(file => file.getName == ".stack-work")).toList
//    val allPackagesMut = mutable.ListBuffer[StackPackage]()
//    StackYaml.fromFile(stackFile.getPath)
//      .leftMap(errorMsg => FileError(stackFile.getCanonicalPath, stackFile.getNameWithoutExtension, errorMsg))
//      .map(yaml => yaml.packages)
//      .map(packages => {
//        val stackPackages = packages.asScala.map(stackPackage => {
//          FileUtil.getVirtualFileDifferentWorkingDir(stackFile.getParent.getCanonicalPath, stackPackage.path)
//            .map(virtualFile => {
//              val children: Array[VirtualFile] = virtualFile.getChildren
//              if (children == null) {
//                Left(FileError(stackPackage.path, virtualFile.getNameWithoutExtension, s"unable to get children for ${stackPackage.path}"))
//              } else {
//                children.toStream
//                  .flatMap(child => CabalPackageManager.getCabalFile(child, project).map((_, child)))
//                  .headOption match {
//                  case Some(tuple) => Right(new StackPackage(tuple._1, tuple._2, stackFile, allPackagesMut, buildDir))
//                  case None => Left(FileError(stackPackage.path, virtualFile.getNameWithoutExtension, s"no Cabal-file found for package ${stackPackage.path}"))
//                }
//              }
//            })
//            .getOrElse(Left(FileError(stackPackage.path, new File(stackPackage.path).getName, s"unable to obtain VirtualFile for stack package ${stackPackage.path}")))
//        }).toList
//        allPackagesMut ++= stackPackages.flatMap(_.right.toOption)
//        stackPackages
//      })
//      .toEither
//  }
//
//  /**
//    * registers the stackPackage if the package is not already registered by another PackageManager
//    * @param stackPackage the package to register
//    * @param project the Intellij Project
//    * @return either an AlreadyRegisteredError or the package
//    */
//  def registerPackage(stackPackage: StackPackage, project: Project): Either[AlreadyRegistered, StackPackage] = {
//    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
//    val added: Boolean = packageManager.addPackage(stackPackage)
//    if (!added) {
//      val registered: Option[HPackage] = packageManager.getPackage(stackPackage.getLocation)
//      if (registered.isEmpty) {
//        LOG.error("unable to register and getPackage returned empty!")
//        Left(AlreadyRegistered(stackPackage))
//      }
//      Left(AlreadyRegistered(registered.get))
//    } else {
//      Right(stackPackage)
//    }
//  }
//
//  /**
//    * registers the stackPackage if the package is not already registered by another PackageManager
//    * @param stackPackage the package to register
//    * @param project the Intellij Project
//    * @return the replaced package if there was already an package registered
//    */
//  def replacePackage(stackPackage: StackPackage, project: Project): Option[HPackage] = {
//    val packageManager: HPackageManager = project.getComponent(classOf[HPackageManager])
//    packageManager.replacePackage(stackPackage)
//  }
//
//  /**
//    * searches the stack-file for packages and replaces/adds them to the PackageManager
//    * @param path the path to the stack.yaml-file
//    * @param project the current Intellij project
//    * @return either an Error if the stack-yaml is not found,
//    *         or a list of packages, where there is an error if the package is not found or the package with the
//    *         optional replaced package
//    */
//  def replacePackages(path: String, project: Project): Either[FileError, List[Either[FileError, (StackPackage, Option[HPackage])]]] = {
//    getPackages(new File(path), project)
//      .right.map(list => {
//      list.map(either => {
//        either.right.map(stackPackage => {
//          (stackPackage, replacePackage(stackPackage, project))
//        })
//      })
//    })
//  }

  /**
    * a unique name of the Package-Manager
    */
  override def getName: String = NAME


  override def getPackageFromState(hPackageState: HPackageState, packageLocation: VirtualFile, project: Project): Option[HPackage] = {
    if (hPackageState.getPackageManager == NAME) {
      try {
        val state: StackPackageState = hPackageState.asInstanceOf[StackPackageState]
        val stackProjectManager: StackProjectManager = StackProjectManager.getInstance(project)
        val optStackFile: Option[VirtualFile] = stackProjectManager.getStackFileFromPath(state.stackPath)
        if (optStackFile.isEmpty) {
          return None
        }
        //TODO register/check stack
        CabalPackageManager.getCabalFile(packageLocation, project).right.toOption
          .map(cabal => new StackPackage(cabal, packageLocation, optStackFile.get, state.stackPath, ListBuffer(), null))
      } catch {
        case exception: ClassCastException => LOG.error(s"hPackageState is not from type StackPackageState", exception)
          None
      }
    } else {
      LOG.error(s"received hPackageState from PackageManager ${hPackageState.getPackageManager}")
      None
    }
  }

  /**
    * returns the Packages
    *
    * @param stackFile the stack file.
    * @return the left Error is an Critical Error,
    *         the right is a list of parsed Packages, where the left packages that are not found etc
    */
  override def getPackages(stackFile: VirtualFile, project: Project): Either[FileError, List[Either[FileError, HPackage]]] = {
    val parent: VirtualFile = stackFile.getParent
    if (parent == null)
      Left(FileError(stackFile.getCanonicalPath, stackFile.getNameWithoutExtension, s"stack file is root directory"))
    val relativeStackFile = FileUtil.toRelativePath(project, stackFile)
    val buildDir = Option(parent.getChildren).flatMap(_.find(file => file.getName == ".stack-work")).toList
    val allPackagesMut = mutable.ListBuffer[StackPackage]()

    StackYaml.fromFile(stackFile.getPath)
      .leftMap(errorMsg => FileError(stackFile.getCanonicalPath, stackFile.getNameWithoutExtension, errorMsg))
      .map(yaml => yaml.packages)
      .map(packages => {
        val stackPackages = packages.asScala.map(stackPackage => {
          FileUtil.fromRelativePath(stackFile.getParent.getCanonicalPath, stackPackage.path)
            .map(virtualFile => {
              val children: Array[VirtualFile] = virtualFile.getChildren
              if (children == null) {
                Left(FileError(stackPackage.path, virtualFile.getNameWithoutExtension, s"unable to get children for ${stackPackage.path}"))
              } else {
                children.toStream
                  .flatMap(child => CabalPackageManager.getCabalFile(child, project).right.toOption.map((_, child)))
                  .headOption match {
                  case Some(tuple) => Right(new StackPackage(tuple._1, tuple._2, stackFile, relativeStackFile, allPackagesMut, buildDir))
                  case None => Left(FileError(stackPackage.path, virtualFile.getNameWithoutExtension, s"no Cabal-file found for package ${stackPackage.path}"))
                }
              }
            })
            .getOrElse(Left(FileError(stackPackage.path, new File(stackPackage.path).getName, s"unable to obtain VirtualFile for stack package ${stackPackage.path}")))
        }).toList
        allPackagesMut ++= stackPackages.flatMap(_.right.toOption)
        stackPackages
      })
      .toEither
  }
}
