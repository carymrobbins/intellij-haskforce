package com.haskforce.haskell.highlighting;

import com.haskforce.haskell.HaskellParserDefinition;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

/**
 * Returns the lexer used for syntax highlighting. The parsing lexer is
 * different and returned by {@link HaskellParserDefinition}.
 */
public class HaskellSyntaxHighlighterFactory extends SyntaxHighlighterFactory {
    @NotNull
    @Override
    public SyntaxHighlighter getSyntaxHighlighter(Project project, VirtualFile virtualFile) {
        return new HaskellSyntaxHighlighter();
    }
}
