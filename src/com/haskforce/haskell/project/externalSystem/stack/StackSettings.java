package com.haskforce.haskell.project.externalSystem.stack;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.externalSystem.settings.AbstractExternalSystemSettings;
import com.intellij.openapi.externalSystem.settings.ExternalSystemSettingsListener;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.annotations.XCollection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Set;
import java.util.TreeSet;

@State(name = "StackSettings", storages = @Storage("haskell-stack.xml"))
public class StackSettings
  extends
    AbstractExternalSystemSettings<
      StackSettings,
      StackProjectSettings,
      StackProjectSettingsListener
    >
  implements PersistentStateComponent<StackSettings.State> {

  public StackSettings(@NotNull Project project) {
    super(StackTopic.get(), project);
  }

  @NotNull
  public static StackSettings getInstance(@NotNull Project project) {
    return project.getService(StackSettings.class);
  }

  @Override
  public void initializeComponent() {
    System.out.println("StackSettings.initializeComponent");
  }

  @Override
  public void subscribe(@NotNull ExternalSystemSettingsListener<StackProjectSettings> listener) {
    doSubscribe(
      new StackDelegatingExternalSystemSettingsListener(listener),
      getProject()
    );
  }

  @Override
  public void subscribe(@NotNull ExternalSystemSettingsListener<StackProjectSettings> listener, @NotNull Disposable parentDisposable) {
    doSubscribe(
      new StackDelegatingExternalSystemSettingsListener(listener),
      parentDisposable
    );
  }

  @Override
  protected void copyExtraSettingsFrom(@NotNull StackSettings settings) {
  }

  @Nullable
  @Override
  public State getState() {
    State state = new State();
    fillState(state);
    return state;
  }

  @Override
  public void loadState(@NotNull State state) {
    super.loadState(state);
  }

  @Override
  protected void checkSettings(@NotNull StackProjectSettings old, @NotNull StackProjectSettings current) {
    if (!old.equals(current)) getPublisher().onStackProjectSettingsChange();
  }

  public static class State
    implements AbstractExternalSystemSettings.State<StackProjectSettings> {

    private final Set<StackProjectSettings> myProjectSettings = new TreeSet<>();

    @Override
    @XCollection(elementTypes = {StackProjectSettings.class})
    public Set<StackProjectSettings> getLinkedExternalProjectsSettings() {
      return myProjectSettings;
    }

    @Override
    public void setLinkedExternalProjectsSettings(
      Set<StackProjectSettings> settings
    ) {
      if (settings == null) return;
      myProjectSettings.addAll(settings);
    }
  }
}
