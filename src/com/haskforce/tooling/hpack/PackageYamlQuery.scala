package com.haskforce.tooling.hpack

import com.intellij.psi.PsiFile

/**
 * TODO: Mostly a hack for us until we get proper stack framework support.
 */
object PackageYamlQuery {

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
