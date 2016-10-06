package com.haskforce.tools.stack.packages

import java.util

import com.haskforce.importWizard.stack.{StackYaml, StackYamlUtil}
import com.haskforce.system.utils.{FileUtil, SAMUtils}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.{PersistentStateComponent, ServiceManager, State}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

import scala.collection.JavaConverters._

/**
  * manages the active stack-projects
  */
@State(
  name = "haskforceStackProjectManager"
)
class StackProjectManager(project: Project, var stackFiles: Set[VirtualFile]) extends PersistentStateComponent[StackProjectsState] {
  def this(project: Project) = this(project, Set())

  override def loadState(state: StackProjectsState): Unit = {
    this.synchronized {
      stackFiles = state.stackFiles.asScala
        .flatMap(location => {
          ApplicationManager.getApplication.runReadAction(SAMUtils.runnable(() => {
            if (StackYaml.fromFile(location).isLeft) {
              None
            }
          }))
          FileUtil.fromRelativePath(location, project.getBasePath)
        })
        .toSet
    }

  }

  override def getState: StackProjectsState = {
    this.synchronized {
      val list = new util.ArrayList(stackFiles.map(file => FileUtil.toRelativePath(project, file)).asJavaCollection)
      new StackProjectsState(list)
    }
  }

  /**
    * returns the file pointing to the StackFile (if registered)
    */
  def getStackFileFromPath(relativePath: String): Option[VirtualFile] = {
    FileUtil.fromRelativePath(relativePath, project.getBasePath)
      .filter(file => stackFiles.contains(file))
  }
}

object StackProjectManager {
  def getInstance(project: Project) = {
    ServiceManager.getService(project, classOf[StackProjectManager])
  }
}


