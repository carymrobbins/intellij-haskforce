package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.project.DumbAware

/**
 * Provides support for auto-importing a Stack project from the UI.
 * Mostly useful for importing an existing IntelliJ project with Stack.
 */
class ImportStackProjectAction extends AnAction with DumbAware {

  override def actionPerformed(e: AnActionEvent): Unit = {
    StackProjectImporter.importProject(
      e.getProject,
      e.getProject.getBasePath
    )
  }
}
