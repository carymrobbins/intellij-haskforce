package com.haskforce.haskell.project.externalSystem.stack;

import com.intellij.openapi.externalSystem.model.ProjectSystemId;
import com.intellij.openapi.externalSystem.settings.AbstractExternalSystemLocalSettings;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

/**
 * Wrapper around {@link AbstractExternalSystemLocalSettings} which makes the
 * protected constructor visible from our Scala code in {@link StackLocalSettings}.
 * There doesn't seem to be a good way to access the constructor directly in
 * Scala, so this wrapper is written minimally in Java.
 */
abstract class AbstractStackLocalSettings<S extends AbstractExternalSystemLocalSettings.State> extends AbstractExternalSystemLocalSettings<S> {
  AbstractStackLocalSettings(@NotNull ProjectSystemId externalSystemId, @NotNull Project project, @NotNull S state) {
    super(externalSystemId, project, state);
  }
}
