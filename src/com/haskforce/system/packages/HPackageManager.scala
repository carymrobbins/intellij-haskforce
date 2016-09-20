package com.haskforce.system.packages

/**
  * this class retrieve, add and remove the active packages
  */
object HPackageManager {
  private var packages : Set[HPackage] = Set()

  /**
    * returns the active Packages
    */
  def getPackages : Set[HPackage] = packages

  /**
    * adds the package to the Set
    * @param hPackage the package to add
    * @return true if added, false if not
    */
  def addPackage(hPackage : HPackage) : Boolean = {
    if (hPackage.getLocation == null) {
      return false
    }
    this.synchronized {
      if (packages.contains(hPackage)) {
        return false
      } else {
        packages = packages + hPackage
        return true
      }
    }
  }

  /**
    * removes the package from the Set
    * @param hPackage the package to remove
    * @return true if removed, false if not
    */
  def removeProject (hPackage : HPackage) : Boolean = {
    this.synchronized {
      if (!packages.contains(hPackage)) {
        return false
      } else {
        packages = packages - hPackage
        return true
      }
    }
  }
}
