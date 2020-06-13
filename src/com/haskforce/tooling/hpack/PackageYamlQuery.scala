package com.haskforce.tooling.hpack

import com.intellij.psi.PsiFile
import org.jetbrains.yaml.YAMLUtil
import org.jetbrains.yaml.psi.YAMLFile

/** TODO: Mostly a hack for us until we get proper package.yaml parser. */
object PackageYamlQuery {

  def getName(x: YAMLFile): Either[Throwable, String] =
    Option(YAMLUtil.getQualifiedKeyInFile(x, "name"))
      .map(_.getValueText)
      .toRight(new NoSuchElementException(s"'name' field in ${x.getName}"))

  def getTopLevelDeps(packageYaml: PsiFile): Option[List[String]] = {
    val res =
      packageYaml.getText.split('\n')
        .iterator
        .dropWhile(_.trim != "dependencies:")
        .drop(1)
        .map(_.trim)
        .takeWhile(s => s == "" || s.startsWith("-"))
        .map(_.stripPrefix("-").trim)
        .filter(_.nonEmpty)
        .toList
    if (res.isEmpty) None else Some(res)
  }
}
