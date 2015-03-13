package com.haskforce.cabal.index;

import com.haskforce.cabal.CabalFileType;
import com.haskforce.cabal.psi.CabalFile;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.indexing.*;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

public class CabalFileIndex extends ScalarIndexExtension<String>{
    private static final ID<String, Void> CABAL_FILE_INDEX = ID.create("CabalFileIndex");
    private static final CabalDataIndexer INDEXER = new CabalDataIndexer();
    private static final EnumeratorStringDescriptor KEY_DESCRIPTOR = new EnumeratorStringDescriptor();
    public static final FileBasedIndex.InputFilter CABAL_MODULE_FILTER = new FileBasedIndex.InputFilter() {
        @Override
        public boolean acceptInput(@NotNull VirtualFile file) {
            //noinspection ObjectEquality
            return file.getFileType() == CabalFileType.INSTANCE && file.isInLocalFileSystem();

        }
    };

    @Nullable
    public static CabalFile getCabalFileByProjectName(@NotNull Project project, @NotNull GlobalSearchScope searchScope) {
        Collection<VirtualFile> containingFiles = FileBasedIndex.getInstance().getContainingFiles(CABAL_FILE_INDEX, project.getName(), searchScope);
        Iterator<VirtualFile> iterator = containingFiles.iterator();
        if (iterator.hasNext()){
            VirtualFile virtualCabalFile = iterator.next();
            return (CabalFile) PsiManager.getInstance(project).findFile(virtualCabalFile);
        }
        return null;
    }

    @NotNull
    @Override
    public ID<String, Void> getName() {
        return CABAL_FILE_INDEX;
    }

    @NotNull
    @Override
    public DataIndexer<String, Void, FileContent> getIndexer() {
        return INDEXER;
    }

    @NotNull
    @Override
    public KeyDescriptor<String> getKeyDescriptor() {
        return KEY_DESCRIPTOR;
    }

    @NotNull
    @Override
    public FileBasedIndex.InputFilter getInputFilter() {
        return CABAL_MODULE_FILTER;
    }

    @Override
    public boolean dependsOnFileContent() {
        return true;
    }

    @Override
    public int getVersion() {
        return 0;
    }

    private static class CabalDataIndexer implements DataIndexer<String, Void, FileContent> {
        @NotNull
        @Override
        public Map<String, Void> map(@NotNull FileContent inputData) {
            PsiFile psiFile = inputData.getPsiFile();
            String projectName = psiFile.getProject().getName();
            //The intput filter should make sure that we only get cabal files in this function
            //Only one cabal file per project supported. Might be we have to change this assumption when
            //trying to support multi module projects. Not practical yet as ghc mod doesn't
            //function over multi module project yet.
            return Collections.singletonMap(projectName,null);
        }
    }
}
