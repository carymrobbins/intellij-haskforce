package com.haskforce.index;

import com.haskforce.HaskellFileType;
import com.haskforce.psi.HaskellFile;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.indexing.FileBasedIndex;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

public final class HaskellFileIndex {

  public static Collection<VirtualFile> getVirtualFiles(@NotNull Project project) {
    return FileBasedIndex.getInstance().getContainingFiles(
      FileTypeIndex.NAME,
      HaskellFileType.INSTANCE,
      GlobalSearchScope.projectScope(project)
    );
  }

  public static List<HaskellFile> getPsiFiles(@NotNull Project project) {
    final PsiManager psiManager = PsiManager.getInstance(project);
    return ContainerUtil.map(
      getVirtualFiles(project),
      new Function<VirtualFile, HaskellFile>() {
        @Override
        public HaskellFile fun(VirtualFile vFile) {
          PsiFile psiFile = psiManager.findFile(vFile);
          if (psiFile == null) {
            throw new AssertionError("Could not find psi file for " + vFile);
          }
          if (psiFile instanceof HaskellFile) return (HaskellFile) psiFile;
          throw new AssertionError("Expected HaskellFile, got " + psiFile + " from " + vFile);
        }
      }
    );
  }
}
