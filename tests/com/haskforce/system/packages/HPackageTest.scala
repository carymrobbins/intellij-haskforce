package com.haskforce.system.packages

import com.haskforce.system.packages.BuildType.{Library, TestSuite}
import com.haskforce.test.AssertMixin
import com.haskforce.tools.cabal.lang.parser.CabalParserTestBase
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.haskforce.tools.cabal.packages.CabalPackage
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import rx.lang.scala.{Subject, Subscription}
import rx.lang.scala.observers.TestSubscriber


class HPackageTest extends CabalParserTestBase with AssertMixin {
  lazy val example00001 = loadHPackage("example00001")
  lazy val example00003 = loadHPackage("example00003")
  lazy val example00005 = loadHPackage("example00005")
  lazy val example00008 = loadHPackage("example00008")
  lazy val sourceDirs00001 = loadHPackage("sourceDirs00001")

  def testGetSources() = {
    example00003.getLibrary.getSourceDirs === example00003.getSources.flatten(_.getSourceDirs)
    sourceDirs00001.getSources.flatten(_.getSourceDirs) === Set(".", "app")
  }

  def testGetTests() = {
    sourceDirs00001.getTests.flatMap(_.getSourceDirs) === Set("tests", "bench1", "bench2")
  }

  def testGetBestMatchingBuildInfo(): Unit = {
    def assertBuildInfo(hPackage: HPackage, string: String, buildType: BuildType) = {
      hPackage.getBestMatchingBuildInfo(string).typ == buildType
    }

    assertBuildInfo(example00001, "/foo.hs", Library)

    assertBuildInfo(example00001, "/tests/foo.hs", TestSuite)

    example00005.getBestMatchingBuildInfo("/tests/foo.hs") == example00005.getLibrary

    assertBuildInfo(example00005, "/src/foo.hs", Library)

    assertBuildInfo(example00005, "/tests/foo.hs", TestSuite)

    assertBuildInfo(example00005, "/examples/foo.hs", Library)
  }

  def testEmitEvent(): Unit = {
    val events: Subject[PackageEvent] = example00001.getEvents
    val subscriber: TestSubscriber[PackageEvent] = TestSubscriber[PackageEvent]()
    events.subscribe(subscriber)
    val remove: Remove = Remove()
    example00001.emitEvent(remove)
    val nextEvents: Seq[PackageEvent] = subscriber.getOnNextEvents
    nextEvents.head === remove
  }

  def loadHPackage(name: String): CabalPackage = {
    val fullName = name + "." + myFileExt
    val fullPath = "tests/gold/tools/cabal/parser/" + fullName
    val path: VirtualFile = LocalFileSystem.getInstance().findFileByPath(fullPath)
    if (path == null) throw new AssertionError(s"VirtualFile for file is null")
    val file: CabalFile = createPsiFile(fullName, loadFile(fullName)) match {
      case f: CabalFile => f
      case other => throw new AssertionError(s"Expected CabalFile but got: $other")
    }
    new CabalPackage(file, path)
  }
}
