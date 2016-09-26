package com.haskforce.tools.cabal;

import com.intellij.lang.Commenter;
import org.jetbrains.annotations.Nullable;

/**
 * Provides the first menu choice "Comment with .." in the Code menu.
 */
public class CabalCommenter implements Commenter {
    @Nullable
    @Override
    public String getLineCommentPrefix() {
        return "--";
    }

    @Nullable
    @Override
    public String getBlockCommentPrefix() {
        return null;
    }

    @Nullable
    @Override
    public String getBlockCommentSuffix() {
        return null;
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentPrefix() {
        return null;
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentSuffix() {
        return null;
    }
}
