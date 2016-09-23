package com.haskforce.system.packages

import com.haskforce.system.utils.ExecUtil
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
  * this class retrieve, add and remove the active packages
  */
trait HPackageManager extends ProjectComponent {
  /**
    * returns the active packages
    */
  def getPackages : Iterable[HPackage]

  /**
    * returns the package at the location
    * @param file the location of the package-defining file (e.g. Cabal-file)
    */
  def getPackage(file: VirtualFile) : Option[HPackage]

  /**
    * the main package (used for ghci etc.)
    * @return the main package or empty if not configured
    */
  def getMainPackage : Option[HPackage]

  /**
    * sets the main package
    */
  def setMainPackage(hPackage: HPackage)

  /**
    * sets the main packages and adds the packages to the Set, replacing if an already registered is found
    * @return an Error, or an tuple with the optional replaced packages and the created one
    */
  def replaceMainPackage(packageManager: PackageManager, file: String) : Either[FileError, (Option[HPackage], HPackage)]

  /**
    * returns the Default GHC-Version
    */
  def getDefaultGHCVersion(project: Project): Either[ExecUtil.ExecError, GHCVersion]

  /**
    * adds the package to the Set, given that there is no other package registered with the same Location
    * @param hPackage the package to add
    * @return true if added, false if not
    */
  def addPackage(hPackage : HPackage) : Boolean

  /**
    * adds the package to the Set, replacing if an already registered is found
    * @param hPackage the package to add
    * @return the old package if replace, or Empty
    */
  def replacePackage(hPackage : HPackage) : Option[HPackage]

  /**
    * removes the package from the Set
    * @param hPackage the package to remove
    * @return true if removed, false if not
    */
  def removePackage(hPackage : HPackage) : Boolean

  /**
    * returns the package the file belongs to
    * @param file the File to query for
    * @return the package if found
    */
  def getPackageForFile(file: VirtualFile): Option[HPackage]
}
