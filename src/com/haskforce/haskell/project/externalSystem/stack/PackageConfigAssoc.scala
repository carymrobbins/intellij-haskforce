package com.haskforce.haskell.project.externalSystem.stack

import com.haskforce.utils.JDOMExternalizable

final case class PackageConfigAssoc(
  packageDir: String,
  packageConfig: PackageConfig
)

object PackageConfigAssoc {

  implicit val jdom: JDOMExternalizable[PackageConfigAssoc] =
    JDOMExternalizable.derive2(apply, unapply)
}
