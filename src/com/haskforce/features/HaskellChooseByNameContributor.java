package com.haskforce.features;

import com.haskforce.utils.HaskellUtil;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiNamedElement;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * The "go to symbol" available on alt-cmd-o.
 */
public class HaskellChooseByNameContributor implements ChooseByNameContributor {
    @NotNull
    @Override
    public String[] getNames(Project project, boolean includeNonProjectItems) {
        List<PsiNamedElement> definitions = HaskellUtil.findDefinitionNodes(project);
        List<String> names = ContainerUtil.newArrayListWithCapacity(definitions.size());
        for (PsiNamedElement def : definitions) {
            if (def.getName() != null && !def.getName().isEmpty()) {
                names.add(def.getName());
            }
        }
        return names.toArray(new String[names.size()]);
    }

    @NotNull
    @Override
    public NavigationItem[] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
        // TODO: include non project items
        List<PsiNamedElement> definitions = HaskellUtil.findDefinitionNode(project, name);
        return definitions.toArray(new NavigationItem[definitions.size()]);
    }
}

