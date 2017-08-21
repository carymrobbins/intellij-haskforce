package prelude.types

import com.intellij.openapi.module.Module
import io.estatico.newtype.NewType

object SModule extends NewType.Default[Module] {
  implicit final class Ops(val self: Type) extends AnyVal {

    def toModule: Module = unwrap(self)

    def getModuleFile: SVirtualFile.Type = {
      val f = toModule.getModuleFile
      if (f == null) throw new IllegalStateException(s"File was null for module: $self")
      SVirtualFile(f)
    }
  }
}
