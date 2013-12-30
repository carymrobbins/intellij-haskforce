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
            new AttributesDescriptor("Reserved Word", HaskellSyntaxHighlighter.RESERVEDID),
            new AttributesDescriptor("Constructor", HaskellSyntaxHighlighter.CONID),
            new AttributesDescriptor("Variable", HaskellSyntaxHighlighter.VARID),
            new AttributesDescriptor("Symbol", HaskellSyntaxHighlighter.VARSYM),
            new AttributesDescriptor("Special", HaskellSyntaxHighlighter.SPECIAL),
            new AttributesDescriptor("String", HaskellSyntaxHighlighter.STRING),
            new AttributesDescriptor("Integer", HaskellSyntaxHighlighter.INTEGER),
            new AttributesDescriptor("Float", HaskellSyntaxHighlighter.FLOAT),
            new AttributesDescriptor("Char", HaskellSyntaxHighlighter.CHAR),
            new AttributesDescriptor("Line Comment", HaskellSyntaxHighlighter.COMMENT),
            new AttributesDescriptor("Block Comment", HaskellSyntaxHighlighter.NCOMMENT),
            new AttributesDescriptor("Doc Comment", HaskellSyntaxHighlighter.HADDOCK),
            new AttributesDescriptor("Escape", HaskellSyntaxHighlighter.ESCAPE)
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
        return  "module AStack( Stack, push, pop, top, size, (<~>) ) where\n" +
                "\n" +
                "import Control.Monad (liftM2)\n" +
                "import Control.Monad.Zip\n" +
                "\n" +
                "data Stack a = Empty\n" +
                "             | MkStack a (Stack a)\n" +
                "\n" +
                "push :: a -> Stack a -> Stack a\n" +
                "push x s = MkStack x s\n" +
                "\n" +
                "size :: Stack a -> Int\n" +
                "size s = length (stkToLst s)  where\n" +
                "           stkToLst  Empty         = []\n" +
                "           stkToLst (MkStack x s)  = x:xs where xs = stkToLst s\n" +
                "\n" +
                "{-\n" +
                " - Here's a multiline comment.\n" +
                " -}\n" +
                "\n" +
                "pop :: Stack a -> (a, Stack a)\n" +
                "pop (MkStack x s)\n" +
                "  = (x, case s of r -> i r where i x = x) -- (pop Empty) is an error\n" +
                "\n" +
                "top :: Stack a -> a\n" +
                "top (MkStack x s) = x                     -- (top Empty) is an error\n" +
                "\n" +
                "instance MonadZip Maybe where\n" +
                "    mzip = liftM2 {- Inline comment. -} (,)\n" +
                "\n" +
                "--^ Pointless operator to zip two Maybe values.\n" +
                "(<~>) :: Maybe a      --^ Just a      | Nothing\n" +
                "      -> Maybe b      --^ Just b      | Nothing\n" +
                "      -> Maybe (a, b) --^ Just (a, b) | Nothing\n" +
                "(<~>) = mzip\n" +
                "\n" +
                "tokenWithPrime' = 1 :: Int\n";
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
