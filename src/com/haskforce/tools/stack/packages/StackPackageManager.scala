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
          FileUtil.fromRelativePath(stackPackage.path, stackFile.getParent.getCanonicalPath)
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
