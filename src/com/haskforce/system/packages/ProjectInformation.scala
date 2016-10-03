package com.haskforce.system.packages

import com.intellij.openapi.vfs.VirtualFile

/**
  */
//TODO does this still make sense?
trait ProjectInformation {
  /**
    * returns a List of directories that can be marked as excluded (if existing)
    */
  def getBuildDirectories: List[VirtualFile]

  /**
    * returns a List of related packages
    */
  def getRelatedPackages: List[HPackage]
}
