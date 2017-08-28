package prelude.types

import com.haskforce.utils.IJReadAction
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiManager
import io.estatico.newtype.NewType

object SPsiManager extends NewType.Of[PsiManager] {

  def getInstance(project: Project): Type = wrap(PsiManager.getInstance(project))

  implicit final class Ops(val self: Type) extends AnyVal {

    def toPsiManager: PsiManager = unwrap(self)

    def findFile(vFile: VirtualFile): IJReadAction[Option[SPsiFile.Top]]
      = IJReadAction(Option(SPsiFile(toPsiManager.findFile(vFile))))
  }
}
