package com.haskforce.system.packages

import java.io.File
import java.util

import com.haskforce.haskell.HaskellSdkType
import com.haskforce.test.AssertMixin
import com.haskforce.tools.stack.packages.StackPackageManager
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.{ModifiableModuleModel, Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ModuleRootManager, ProjectRootManager}
import com.intellij.openapi.util.Computable
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.testFramework.PlatformTestCase
import com.intellij.testFramework.fixtures.{LightCodeInsightFixtureTestCase, LightPlatformCodeInsightFixtureTestCase}
import org.jetbrains.jps.model.java.JavaSourceRootType
import org.jetbrains.jps.model.module.JpsModuleSourceRootType

import scala.collection.JavaConversions._
import scalaz.syntax.id._

class ProjectSetupTest extends PlatformTestCase with AssertMixin {

  val testDir = "tests/gold/importWizard/stack"

  def setupModules() = {
    val project: Project = getProject
    val projectDir = s"$testDir/stack-ide"
    val stackFile: VirtualFile = LocalFileSystem.getInstance().findFileByPath(projectDir + "/stack.yaml")
    assertNotNull(stackFile)
    val errorOrResults: Either[FileError, List[Either[FileError, HPackage]]] = StackPackageManager.getPackages(stackFile, project)
    assertTrue(errorOrResults.isRight)
    val errorOrPackages: List[Either[FileError, HPackage]] = errorOrResults.right.get
    val errors = errorOrPackages.flatMap(_.left.toOption)
    assertTrue(errors.isEmpty)
    val packages: List[HPackage] = errorOrPackages.flatMap(_.right.toOption)
    val modifiableModel: ModifiableModuleModel = ModuleManager.getInstance(project).getModifiableModel
    val modules: List[ChangedModule] = ApplicationManager.getApplication.runWriteAction(new Computable[List[ChangedModule]] {
      override def compute(): List[ChangedModule] = {
        val (existing, updated, created) = ProjectSetup.setUpWithUpdate(packages, project, modifiableModel, projectDir, setupRoot = true)
        assertTrue(existing.isEmpty)
        created ++ updated
      }
    })
    assertTrue(modules.nonEmpty)
    modules
  }

  lazy val changedModules: List[ChangedModule] = setupModules()

  def testSetupModuleModel() = {
    val changedModules: List[ChangedModule] = this.changedModules
    val project: Project = getProject
    val projectDir = s"$testDir/stack-ide"
    val canonicalProjectDir = new File(projectDir).getCanonicalPath
    val paths = changedModules.map(_.module).map(_.getModuleFilePath)
    val modules: List[Module] = changedModules.map(_.module)

    val expected = util.Arrays.asList(
      "ide-backend/ide-backend-common/ide-backend-common.iml",
      "ide-backend/ide-backend/ide-backend.iml",
      "ide-backend/ide-backend-server/ide-backend-server.iml",
      "stack-ide-api/stack-ide-api.iml",
      "stack-ide/stack-ide.iml",
      "stack-ide (root).iml"
    )

    expected.foreach { p =>
      assertTrue(s"unable to find existing module for expected path $p", paths.exists(canidate => canidate.endsWith(p)))
    }

    changedModules.filter(_.hPackage.isDefined).map(_.module).foreach { m =>
      assertInstanceOf[HaskellSdkType](
        s"Expected module '$m' SdkType to be HaskellSdkType",
        ModuleRootManager.getInstance(m).getSdk.getSdkType
      )
    }

    assertSome(modules.find(_.getName == "stack-ide-api")) |> { m =>
      assertSameElements(ProjectSetupTestHelper.getSourceDirPaths(m), s"$canonicalProjectDir/stack-ide-api/src")
      assertEmpty(ProjectSetupTestHelper.getTestSourceDirPaths(m))
    }
    assertSome(modules.find(_.getName == "stack-ide")) |> { m =>
      assertSameElements(ProjectSetupTestHelper.getSourceDirPaths(m),
        s"$canonicalProjectDir/stack-ide/src",
        s"$canonicalProjectDir/stack-ide/src/main"
      )
      assertEmpty(ProjectSetupTestHelper.getTestSourceDirPaths(m))
    }
    assertSome(modules.find(_.getName == "ide-backend")) |> { m =>
      assertSameElements(ProjectSetupTestHelper.getSourceDirPaths(m), s"$canonicalProjectDir/ide-backend/ide-backend")
      assertSameElements(ProjectSetupTestHelper.getTestSourceDirPaths(m),
        s"$canonicalProjectDir/ide-backend/ide-backend/TestSuite",
        s"$canonicalProjectDir/ide-backend/ide-backend/test"
      )
    }
    assertSome(modules.find(_.getName == "stack-ide (root)")) |> { m =>
      assertEmpty(ProjectSetupTestHelper.getSourceDirPaths(m))
      assertEmpty(ProjectSetupTestHelper.getTestSourceDirPaths(m))
    }
  }

  def testRegisterHPackages() = {
    val changedModules: List[ChangedModule] = this.changedModules
    val project: Project = getProject
    val modules: Array[Module] = ModuleManager.getInstance(project).getModules
    val registeredPackages: Array[HPackage] = modules.flatMap(m => HPackageModule.getInstance(m).optPackage)
    assertEquals(registeredPackages.length, 5)
    assertTrue("stack-ide must be registered", registeredPackages.flatMap(_.getName).contains("stack-ide"))
  }
}

object ProjectSetupTestHelper {
  def getSourceRoots(m: Module, typ: JpsModuleSourceRootType[_]): util.List[VirtualFile] = {
    ModuleRootManager.getInstance(m).getSourceRoots(typ)
  }

  def getSourceRootPaths(m: Module, typ: JpsModuleSourceRootType[_]): util.List[String] = {
    ProjectSetupTestHelper.getSourceRoots(m, typ).map(_.getPath)
  }

  def getSourceDirPaths(m: Module): util.List[String] = {
    ProjectSetupTestHelper.getSourceRootPaths(m, JavaSourceRootType.SOURCE)
  }

  def getTestSourceDirPaths(m: Module): util.List[String] = {
    ProjectSetupTestHelper.getSourceRootPaths(m, JavaSourceRootType.TEST_SOURCE)
  }
}
