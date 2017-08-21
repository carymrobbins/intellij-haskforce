package prelude.types

import com.intellij.openapi.vfs.VirtualFile
import io.estatico.newtype.NewType

object SVirtualFile extends NewType.Default[VirtualFile] {
  implicit final class Ops(val self: Type) extends AnyVal {

    def toVirtualFile: VirtualFile = unwrap(self)

    def getParent: Option[Type] = Option(SVirtualFile(toVirtualFile.getParent))

    def getCanonicalPath: Option[String] = Option(toVirtualFile.getCanonicalPath)

    def getChildren: Option[Array[Type]] = Option(applyM(toVirtualFile.getChildren))

    def getExtension: Option[String] = Option(toVirtualFile.getExtension)
  }
}
