package com.haskforce.system.integrations.highlighting;

import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.xml.util.XmlStringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.regex.Pattern;

public class HaskellAnnotationHolder {
    public final AnnotationHolder holder;

    public HaskellAnnotationHolder(@NotNull AnnotationHolder holder) {
        this.holder = holder;
    }

    @Nullable
    public Annotation createInfoAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.INFORMATION, range, message);
    }

    @Nullable
    public Annotation createWeakWarningAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WEAK_WARNING, range, message);
    }

    @Nullable
    public Annotation createWarningAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WARNING, range, message);
    }

    @Nullable
    public Annotation createErrorAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.ERROR, range, message);
    }

    @Nullable
    private Annotation createAnnotation(@NotNull HighlightSeverity severity, @NotNull TextRange range, @Nullable String message) {
        // Skip duplicate messages.
        if (!messages.add(message)) return null;
        String tooltip = message == null ? null : XmlStringUtil.wrapInHtml(escapeSpacesForHtml(XmlStringUtil.escapeString(message)));
        return holder.createAnnotation(severity, range, message, tooltip);
    }

    private static final Pattern NEWLINE_REGEX = Pattern.compile("\n");
    private static final Pattern SPACE_REGEX = Pattern.compile(" ");

    public static String escapeSpacesForHtml(String string) {
        return string == null ? null : SPACE_REGEX.matcher(NEWLINE_REGEX.matcher(string).replaceAll("<br/>")).replaceAll("&nbsp;");
    }

    /**
     * Keep track of what messages we are displaying so we won't have duplicates.
     */
    private HashSet<String> messages = new HashSet<String>();
}
