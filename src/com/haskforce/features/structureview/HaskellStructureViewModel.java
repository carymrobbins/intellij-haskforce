package com.haskforce.features.structureview;

import com.haskforce.psi.HaskellFile;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.StructureViewModelBase;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.Sorter;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

/**
 *
 */
public class HaskellStructureViewModel extends StructureViewModelBase implements
        StructureViewModel.ElementInfoProvider {
    public HaskellStructureViewModel(PsiFile psiFile) {
        super(psiFile, new HaskellStructureViewElement(psiFile));
    }

    @NotNull
    public Sorter[] getSorters() {
        return new Sorter[] {Sorter.ALPHA_SORTER};
    }

    @Override
    public boolean isAlwaysShowsPlus(StructureViewTreeElement element) {
        return false;
    }

    @Override
    public boolean isAlwaysLeaf(StructureViewTreeElement element) {
        return element instanceof HaskellFile;
    }
}