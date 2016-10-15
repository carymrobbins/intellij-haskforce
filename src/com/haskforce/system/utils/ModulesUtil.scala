package com.haskforce.system.utils

import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
  */
object ModulesUtil {
  /**
    * returns all the Modules the file could belong to (may require adding a content-root)
    */
  def getMatchingModules(file: VirtualFile, project: Project): List[Module] = {
    ModuleManager.getInstance(project).getModules.toList
      .filter(module => {
        module.getModuleContentScope.contains(file)
      })
  }
}
