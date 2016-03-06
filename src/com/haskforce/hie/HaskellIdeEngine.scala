package com.haskforce.hie

import com.haskforce.settings.ToolKey
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.VisualPosition
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project


/*
 * Interface for Haskell Ide Engine (hie)
 */
class HaskellIdeEngine private(path: String, flags: String) {
  val LOG = Logger.getInstance(getClass);

  def typeAt(module: Module, canonicalPath: String, blockStart: VisualPosition, blockEnd: VisualPosition): String = {
    LOG.warn("HaskellIdeEngine.typeAt called");
    val cmd = new GeneralCommandLine()
        .withWorkDirectory(module.getModuleFilePath)
        .withExePath(path)
        .withParameters(flags)

    "Hello from hie";
  }
}

object HaskellIdeEngine {
  val LOG = Logger.getInstance(getClass);

  def apply(project: Project): Option[HaskellIdeEngine] = {
    LOG.warn("creating hie");
    val path = ToolKey.HASKELL_IDE_ENGINE.getPath(project);
    if(path == null){
      LOG.warn("hie path was null");
      return None;
    }
    val flags = ToolKey.HASKELL_IDE_ENGINE.getFlags(project);
    Some(new HaskellIdeEngine(path, flags));
  }
}