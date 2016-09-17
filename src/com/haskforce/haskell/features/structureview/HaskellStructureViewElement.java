package com.haskforce.haskell.features.structureview;

import com.haskforce.haskell.psi.HaskellFile;
import com.haskforce.utils.HaskellUtil;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.SortableTreeElement;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Node in the structure view.
 */
public class HaskellStructureViewElement implements StructureViewTreeElement, SortableTreeElement {
    private PsiElement element;

    public HaskellStructureViewElement(PsiElement element) {
        this.element = element;
    }

    @Override
    public Object getValue() {
        return element;
    }

    @Override
    public void navigate(boolean requestFocus) {
        if (element instanceof NavigationItem) {
            ((NavigationItem) element).navigate(requestFocus);
        }
    }

    @Override
    public boolean canNavigate() {
        return element instanceof NavigationItem &&
                ((NavigationItem)element).canNavigate();
    }

    @Override
    public boolean canNavigateToSource() {
        return element instanceof NavigationItem &&
                ((NavigationItem)element).canNavigateToSource();
    }

    @NotNull
    @Override
    public String getAlphaSortKey() {
        if (element instanceof PsiNamedElement) {
            String name = ((PsiNamedElement) element).getName();
            return name == null ? "" : name;
        }
        return "";
    }

    @NotNull
    @Override
    public ItemPresentation getPresentation() {
        return element instanceof NavigationItem ?
                ((NavigationItem) element).getPresentation() : null;
    }

    /**
     * Populates the structure view. Uses HaskellUtil to get backing information.
     */
    @NotNull
    @Override
    public TreeElement[] getChildren() {
        if (element instanceof HaskellFile) {
            List<PsiNamedElement> elems =
                    HaskellUtil.findDefinitionNodes((HaskellFile) element.getContainingFile(),
                            null);
            List<TreeElement> treeElems = ContainerUtil.newArrayListWithCapacity(elems.size());
            for (PsiNamedElement elem : elems) {
                //noinspection ObjectAllocationInLoop
                treeElems.add(new HaskellStructureViewElement(elem));
            }
            return treeElems.toArray(new TreeElement[treeElems.size()]);
        }
        return EMPTY_ARRAY;
    }
}

