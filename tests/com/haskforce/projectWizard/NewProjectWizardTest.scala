package com.haskforce.projectWizard

import scala.concurrent.duration._

import com.intellij.ide.projectWizard.{ProjectSettingsStep, NewProjectWizardTestCase, ProjectTypeStep}
import com.intellij.ide.wizard.Step
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.roots.{ModuleRootManager, ProjectRootManager}

import com.haskforce.Implicits._
import com.haskforce._
import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.test.AssertMixin
import com.haskforce.utils.Logging

/**
 * Tests for creating new Haskell projects.
 */
class NewProjectWizardTest extends NewProjectWizardTestCase with AssertMixin with Logging {

  def testNewStackProject(): Unit = {
    val projectName = "myProject"
    val project = createProject { step: Step =>
      val projectTypeStep = assertInstanceOf[ProjectTypeStep](step)
      assertTrue(projectTypeStep.setSelectedTemplate("Haskell", null))
      assertEquals(5, myWizard.getSequence.getSelectedSteps.size)

      val buildToolForm = nextStep[HaskellBuildToolStep]().form
      buildToolForm.buildWithStackRadio.setSelected(true)
      assertFalse(buildToolForm.buildWithCabalRadio.isSelected)
      // We should have stack installed, and it should be inferred properly.
      assertExecutable(buildToolForm.stackPathField.getText)

      val cabalStepForm = nextStep[HaskellCabalPackageSettingsStep]().form
      assertTrue(cabalStepForm.shouldInitializeCabalPackage)
      cabalStepForm.categoryField.setSelectedItem("Web")

      val projectStep = nextStep[ProjectSettingsStep]()
      projectStep.getModuleNameField.setText(projectName)
      assertTrue(myWizard.doFinishAction())
    }

    // Assert that the project name matches.
    assertEquals(projectName, project.getName)

    // Assert that the project sdk type is Haskell.
    val projectRootManager = ProjectRootManager.getInstance(project)
    assertInstanceOf[HaskellSdkType](projectRootManager.getProjectSdk.getSdkType)

    // Assert that one module was created and it has a Haskell sdk.
    val modules = ModuleManager.getInstance(project).getModules
    assertEquals(1, modules.size)
    val module = modules.head
    val moduleRootManager = ModuleRootManager.getInstance(module)
    assertInstanceOf[HaskellSdkType](moduleRootManager.getSdk.getSdkType)

    // Assert that the build settings are properly configured.
    val buildSettings = HaskellBuildSettings.getInstance(project)
    assertFalse(buildSettings.isCabalEnabled)
    assertTrue(buildSettings.isStackEnabled)
    assertEquals(project.getBasePath + "/stack.yaml", buildSettings.getStackFile)

    // Assert that the cabal, Setup.hs, and stack.yaml files were created.
    val contentRoot = moduleRootManager.getContentRoots.head
    val cabalFile = Option(contentRoot.findChild("myProject.cabal"))
    assertSome(cabalFile)(_.exists())
    val setupFile = Option(contentRoot.findChild("Setup.hs"))
    assertSome(setupFile)(_.exists())
    // TODO: This doesn't work on CI
    // See: https://github.com/carymrobbins/intellij-haskforce/issues/235
    // Wait at least 1 second for `stack init` to finish.
    // pollAssert(100.millis, 1.minute) { duration =>
    //   contentRoot.refresh(false, true)
    //   val stackFile = Option(contentRoot.findChild("stack.yaml"))
    //   assertSome(stackFile)(_.exists())
    //   println(s"Found stack.yaml in ${duration.toMillis} ms")
    // }
  }

  private def nextStep[A <: Step : Manifest](): A = {
    myWizard.doNextAction()
    assertInstanceOf[A](myWizard.getCurrentStepObject)
  }
}
