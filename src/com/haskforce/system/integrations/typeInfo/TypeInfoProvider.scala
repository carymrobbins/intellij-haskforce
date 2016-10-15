package com.haskforce.system.integrations.typeInfo

import com.haskforce.tools.ghcmod.TypeInfoUtil
import com.intellij.openapi.editor.VisualPosition
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
  */
trait TypeInfoProvider {
  def getTypeInfo(project: Project): String

  def getTypeInfo(module: Module, blockStart: VisualPosition, blockEnd: VisualPosition, projectFile: VirtualFile): String
}

object TypeInfo {
  val ghcMod = new TypeInfoUtil

  def getTypeInfo(project: Project): String = ghcMod.getTypeInfo(project)

  def getTypeInfo(module: Module, blockStart: VisualPosition,
                  blockEnd: VisualPosition, projectFile: VirtualFile): String = ghcMod.getTypeInfo(module, blockStart, blockEnd, projectFile)
}