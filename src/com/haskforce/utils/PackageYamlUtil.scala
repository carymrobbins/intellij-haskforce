package com.haskforce.utils

import org.jetbrains.yaml.YAMLUtil
import org.jetbrains.yaml.psi.YAMLFile

object PackageYamlUtil {

  def getName(x: YAMLFile): Either[Throwable, String] =
    Option(YAMLUtil.getQualifiedKeyInFile(x, "name"))
      .map(_.getValueText)
      .toRight(new NoSuchElementException(s"'name' field in ${x.getName}"))
}
