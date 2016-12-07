package com.haskforce.codeInsight;

import com.haskforce.HaskellIcons;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.daemon.RelatedItemLineMarkerInfo;
import com.intellij.codeInsight.daemon.RelatedItemLineMarkerProvider;
import com.intellij.codeInsight.navigation.NavigationGutterIconBuilder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

/**
 * A disabled LineMarkerProvider. Will fill the left icon tray with Haskell
 * icons if enabled. Mainly useful for debugging reference resolving.
 */
public class HaskellLineMarkerProvider extends RelatedItemLineMarkerProvider {
    @Override
    protected void collectNavigationMarkers(@NotNull PsiElement element,
                            Collection<? super RelatedItemLineMarkerInfo> result) {
        if (false && element instanceof PsiNamedElement) {
            PsiNamedElement namedElement = (PsiNamedElement) element;
            String value = namedElement.getName();
            if (value != null) {
                Project project = element.getProject();
                final List<HaskellUtil.FoundDefinition> found =
                        HaskellUtil.findDefinitionNode(project, value, namedElement);
                final List<PsiNamedElement> namedNodes = ContainerUtil.newArrayList();
                for (HaskellUtil.FoundDefinition fd : found) {
                    namedNodes.add(fd.element);
                }

                if (namedNodes.size() > 0) {
                    NavigationGutterIconBuilder<PsiElement> builder =
                            NavigationGutterIconBuilder.create(HaskellIcons.FILE).
                                    setTargets(namedNodes).
                                    setTooltipText("Navigate to element definition");
                    result.add(builder.createLineMarkerInfo(element));
                }
            }
        }
    }
}
