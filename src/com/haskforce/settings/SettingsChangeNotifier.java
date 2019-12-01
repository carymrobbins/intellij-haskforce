package com.haskforce.settings;

import com.intellij.util.messages.Topic;
import org.jetbrains.annotations.NotNull;

public interface SettingsChangeNotifier {
    Topic<SettingsChangeNotifier> GHC_MODI_TOPIC = Topic.create("ghc-modi", SettingsChangeNotifier.class);
    Topic<SettingsChangeNotifier> HSDEV_TOPIC = Topic.create("hsdev", SettingsChangeNotifier.class);

    void onSettingsChanged(@NotNull ToolSettings settings);
}
