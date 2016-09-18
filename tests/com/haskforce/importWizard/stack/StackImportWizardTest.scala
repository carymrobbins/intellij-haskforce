package com.haskforce.importWizard.stack

import java.io.File
import java.util

import scala.collection.JavaConversions._
import scalaz.syntax.id._
import com.intellij.ide.projectWizard.ProjectWizardTestCase
import com.intellij.ide.util.newProjectWizard.AddModuleWizard
import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.{ModuleRootManager, ProjectRootManager}
import com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.jps.model.java.JavaSourceRootType
import org.jetbrains.jps.model.module.JpsModuleSourceRootType
import com.haskforce.test.AssertMixin
import com.haskforce.tools.stack.importWizard.{StackProjectImportBuilder, StackProjectImportProvider}
import com.haskforce.haskell.{HaskellModuleType, HaskellSdkType}

/** Tests for importing stack projects and modules. */
class StackImportWizardTest extends ProjectWizardTestCase[AddModuleWizard] with AssertMixin {

  val testDir = "tests/gold/importWizard/stack"

  def testImportProjectDirectory(): Unit = {
    val projectDir = s"$testDir/stack-ide"
    val canonicalProjectDir = new File(projectDir).getCanonicalPath
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
    ).map(path => s"$canonicalProjectDir/$path")

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

    assertSome(modules.find(_.getName == "stack-ide-api")) |> { m =>
      assertSameElements(getSourceDirPaths(m), s"$canonicalProjectDir/stack-ide-api/src")
      assertEmpty(getTestSourceDirPaths(m))
    }
    assertSome(modules.find(_.getName == "stack-ide")) |> { m =>
      assertSameElements(getSourceDirPaths(m),
        s"$canonicalProjectDir/stack-ide/src",
        s"$canonicalProjectDir/stack-ide/src/main"
      )
      assertEmpty(getTestSourceDirPaths(m))
    }
    assertSome(modules.find(_.getName == "ide-backend")) |> { m =>
      assertSameElements(getSourceDirPaths(m), s"$canonicalProjectDir/ide-backend/ide-backend")
      assertSameElements(getTestSourceDirPaths(m),
        s"$canonicalProjectDir/ide-backend/ide-backend/TestSuite",
        s"$canonicalProjectDir/ide-backend/ide-backend/test"
      )
    }
    assertSome(modules.find(_.getName == "stack-ide (root)")) |> { m =>
      assertEmpty(getSourceDirPaths(m))
      assertEmpty(getTestSourceDirPaths(m))
    }
  }

  /** Imports a simple stack project created from 'stack new' */
  def testImportSimpleProject(): Unit = {
    val projectDir = s"$testDir/simple"
    val canonicalProjectDir = new File(projectDir).getCanonicalPath
    val project = importProjectFrom(projectDir, null, newImportProvider()).getProject
    val modules = HaskellModuleType.findModules(project)
    val paths = modules.map(_.getModuleFilePath)

    val expected = util.Arrays.asList(
      "simple.iml"
    ).map(path => s"$canonicalProjectDir/$path")

    assertInstanceOf[HaskellSdkType](
      "expected project SdkType to be HaskellSdkType",
      ProjectRootManager.getInstance(project).getProjectSdk.getSdkType
    )

    assertSameElements("Could not find module file(s)", paths, expected)

    modules.foreach { m =>
      assertInstanceOf[HaskellSdkType](
        s"Expected module '$m' SdkType to be HaskellSdkType",
        ModuleRootManager.getInstance(m).getSdk.getSdkType
      )
      assertSameElements(
        getSourceDirPaths(m),
        s"$canonicalProjectDir/src",
        s"$canonicalProjectDir/app"
      )
      assertSameElements(
        getTestSourceDirPaths(m),
        s"$canonicalProjectDir/test"
      )
    }
  }

  private def newImportProvider() = {
    new StackProjectImportProvider(new StackProjectImportBuilder)
  }

  private def getSourceRoots(m: Module, typ: JpsModuleSourceRootType[_]): util.List[VirtualFile] = {
    ModuleRootManager.getInstance(m).getSourceRoots(typ)
  }

  private def getSourceRootPaths(m: Module, typ: JpsModuleSourceRootType[_]): util.List[String] = {
    getSourceRoots(m, typ).map(_.getPath)
  }

  private def getSourceDirPaths(m: Module): util.List[String] = {
    getSourceRootPaths(m, JavaSourceRootType.SOURCE)
  }

  private def getTestSourceDirPaths(m: Module): util.List[String] = {
    getSourceRootPaths(m, JavaSourceRootType.TEST_SOURCE)
  }
}
