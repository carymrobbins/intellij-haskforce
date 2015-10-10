package com.haskforce.importWizard.stack

import java.io.File
import java.util

import scala.collection.JavaConversions._

import com.intellij.ide.projectWizard.ProjectWizardTestCase
import com.intellij.ide.util.newProjectWizard.AddModuleWizard
import com.intellij.openapi.roots.{ModuleRootManager, ProjectRootManager}

import com.haskforce.test.AssertMixin
import com.haskforce.{HaskellModuleType, HaskellSdkType}

/**
 * Tests for importing stack projects and modules.
 */
class StackImportWizardTest extends ProjectWizardTestCase[AddModuleWizard] with AssertMixin {

  val testDir = "tests/gold/importWizard/stack"

  def testImportProjectDirectory(): Unit = {
    val projectDir = s"$testDir/stack-ide"
    val project = importProjectFrom(projectDir, null, newImportProvider()).getProject
    val modules = HaskellModuleType.findModules(project)
    val paths = modules.map(_.getModuleFilePath)

    val expected = util.Arrays.asList(
      "ide-backend/ide-backend-common/ide-backend-common.iml",
      "ide-backend/ide-backend/ide-backend.iml",
      "ide-backend/ide-backend-server/ide-backend-server.iml",
      "stack-ide-api/stack-ide-api.iml",
      "stack-ide/stack-ide.iml",
      "stack-ide (root).iml"
    ).map(path => new File(s"$projectDir/$path").getCanonicalPath)

    assertInstanceOf[HaskellSdkType](
      "Expected project SdkType to be HaskellSdkType",
      ProjectRootManager.getInstance(project).getProjectSdk.getSdkType
    )

    assertSameElements("Could not find module file(s)", paths, expected)

    modules.foreach { m =>
      assertInstanceOf[HaskellSdkType](
        s"Expected module '$m' SdkType to be HaskellSdkType",
        ModuleRootManager.getInstance(m).getSdk.getSdkType
      )
    }
  }

  private def newImportProvider() = {
    new StackProjectImportProvider(new StackProjectImportBuilder)
  }
}
