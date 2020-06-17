package com.haskforce.haskell.project.externalSystem.stack

import java.util
import java.util.regex.Pattern

import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.service.settings.ExternalSystemConfigLocator
import com.intellij.openapi.externalSystem.settings.ExternalProjectSettings
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}

class StackConfigLocator extends ExternalSystemConfigLocator {

  override def getTargetExternalSystemId: ProjectSystemId = {
    StackManager.PROJECT_SYSTEM_ID
  }

  override def adjust(configPath: VirtualFile): VirtualFile = {
    // TODO: This is a guess, I have no idea.
    if (configPath.isDirectory) {
      configPath.findChild("stack.yaml") match {
        case null => null
        case stackYaml => stackYaml
      }
    } else {
      configPath
    }
  }

  override def findAll(externalProjectSettings: ExternalProjectSettings): util.List[VirtualFile] = {
    // TODO: This is a guess, I have no idea.
    val res = new util.ArrayList[VirtualFile]()
    val localFileSystem = LocalFileSystem.getInstance()

    externalProjectSettings.getModules.forEach { path =>
      val vFile = localFileSystem.refreshAndFindFileByPath(path)
      if (vFile != null) {
        vFile.getChildren.foreach { child =>
          if (StackConfigLocator.isPossibleConfigFile(child)) {
            res.add(child)
          }
        }
      }
    }
    res
  }
}

object StackConfigLocator {

  private val configFileRegex = Pattern.compile("^(package\\.yaml|stack\\.yaml|.*\\.cabal)$")

  private def isPossibleConfigFile(vFile: VirtualFile): Boolean = {
    !vFile.isDirectory && configFileRegex.matcher(vFile.getName).matches()
  }
}
