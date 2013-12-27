package com.haskforce;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

public class HaskellColorSettingsPage implements ColorSettingsPage {
    private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[] {
            new AttributesDescriptor("Key", HaskellSyntaxHighlighter.KEY),
            new AttributesDescriptor("Separator", HaskellSyntaxHighlighter.SEPARATOR),
            new AttributesDescriptor("Value", HaskellSyntaxHighlighter.VALUE),
    };

    @Nullable
    @Override
    public Icon getIcon() {
        return HaskellIcons.FILE;
    }

    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return new HaskellSyntaxHighlighter();
    }

    @NotNull
    @Override
    public String getDemoText() {
        return "module Foo.Bar where\n" +
               "\n" +
               "import Control.Monad (liftM2)\n" +
               "import Control.Monad.Zip (mzip)\n" +
               "\n" +
               "class (Show a) => Foo a where\n" +
               "    foo :: a -> String\n" +
               "    foo = (++ \"Foo\") . show\n" +
               "\n" +
               "instance Foo Int where\n" +
               "    foo = (\"DoubleFoo\" ++) . show . (*2)\n" +
               "\n" +
               "instance MonadZip Maybe where\n" +
               "    mzip = liftM2 (,)\n" +
               "\n" +
               "(<~>) :: Maybe a -> Maybe b -> Maybe (a, b)\n" +
               "(<~>) = mzip\n";
    }

    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }

    @NotNull
    @Override
    public AttributesDescriptor[] getAttributeDescriptors() {
        return DESCRIPTORS;
    }

    @NotNull
    @Override
    public ColorDescriptor[] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @NotNull
    @Override
    public String getDisplayName() {
        return "Haskell";
    }
}
