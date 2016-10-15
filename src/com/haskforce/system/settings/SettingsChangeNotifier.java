package com.haskforce.system.settings;

import com.intellij.util.messages.Topic;
import org.jetbrains.annotations.NotNull;

public interface SettingsChangeNotifier {
    Topic<SettingsChangeNotifier> GHC_MODI_TOPIC = Topic.create("ghc-modi", SettingsChangeNotifier.class);

    void onSettingsChanged(@NotNull ToolSettings settings);
}
