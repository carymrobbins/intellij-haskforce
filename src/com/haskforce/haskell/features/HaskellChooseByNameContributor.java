package com.haskforce.haskell.features;

import com.haskforce.haskell.psi.HaskellNamedElement;
import com.haskforce.haskell.stubs.index.HaskellAllNameIndex;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

/**
 * The "go to symbol" available on alt-cmd-o.
 */
public class HaskellChooseByNameContributor implements ChooseByNameContributor {
    @NotNull
    @Override
    public String[] getNames(Project project, boolean includeNonProjectItems) {
        return ArrayUtil.toStringArray(StubIndex.getInstance().getAllKeys(HaskellAllNameIndex.KEY, project));
    }

    @NotNull
    @Override
    public NavigationItem[] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
        GlobalSearchScope scope = includeNonProjectItems ? GlobalSearchScope.allScope(project) : GlobalSearchScope.projectScope(project);
        Collection<HaskellNamedElement> result = StubIndex.getElements(HaskellAllNameIndex.KEY, name, project, scope, HaskellNamedElement.class);
        List<NavigationItem> items = ContainerUtil.newArrayListWithCapacity(result.size());
        for (HaskellNamedElement element : result) {
            items.add(element);
        }
        return items.toArray(new NavigationItem[items.size()]);
    }
}
