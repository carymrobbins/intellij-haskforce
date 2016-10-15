package com.haskforce.system.integrations.highlighting;

import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.xml.util.XmlStringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.regex.Pattern;

/**
 * holds all the Annotations for an PsiFile
 */
public class HaskellAnnotationHolder {
    private static final Pattern NEWLINE_REGEX = Pattern.compile("\n");
    private static final Pattern SPACE_REGEX = Pattern.compile(" ");

    public final AnnotationHolder holder;
    /**
     * Keep track of what messages we are displaying so we won't have duplicates.
     */
    private HashSet<String> messages = new HashSet<String>();

    public HaskellAnnotationHolder(@NotNull AnnotationHolder holder) {
        this.holder = holder;
    }

    /**
     * creates an Annotation with the Information severity
     * @param range the text-range
     * @param message the message, displayed as plain-text
     * @return the Annotation or null
     */
    @Nullable
    public Annotation createInfoAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.INFORMATION, range, message);
    }

    /**
     * creates an Annotation with the Weak-Warning severity
     * @param range the text-range
     * @param message the message, displayed as plain-text
     * @return the Annotation or null
     */
    @Nullable
    public Annotation createWeakWarningAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WEAK_WARNING, range, message);
    }

    /**
     * creates an Annotation with the Warning severity
     * @param range the text-range
     * @param message the message, displayed as plain-text
     * @return the Annotation or null
     */
    @Nullable
    public Annotation createWarningAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.WARNING, range, message);
    }

    /**
     * creates an Annotation with the Error severity
     * @param range the text-range
     * @param message the message, displayed as plain-text
     * @return the Annotation or null
     */
    @Nullable
    public Annotation createErrorAnnotation(@NotNull TextRange range, @Nullable String message) {
        return createAnnotation(HighlightSeverity.ERROR, range, message);
    }

    /**
     * creates an Annotation
     * @param severity the severity
     * @param range the text-range
     * @param message the message, displayed as plain-text
     * @return the Annotation or null
     */
    @Nullable
    private Annotation createAnnotation(@NotNull HighlightSeverity severity, @NotNull TextRange range, @Nullable String message) {
        // Skip duplicate messages.
        if (!messages.add(message)) return null;
        String tooltip = message == null ? null : XmlStringUtil.wrapInHtml(escapeSpacesForHtml(XmlStringUtil.escapeString(message)));
        return holder.createAnnotation(severity, range, message, tooltip);
    }

    public static String escapeSpacesForHtml(String string) {
        return string == null ? null : SPACE_REGEX.matcher(NEWLINE_REGEX.matcher(string).replaceAll("<br/>")).replaceAll("&nbsp;");
    }
}
