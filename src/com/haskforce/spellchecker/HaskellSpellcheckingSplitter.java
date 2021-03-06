package com.haskforce.spellchecker;

import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.spellchecker.inspections.BaseSplitter;
import com.intellij.spellchecker.inspections.PlainTextSplitter;
import com.intellij.spellchecker.inspections.Splitter;
import com.intellij.spellchecker.inspections.TextSplitter;
import com.intellij.util.Consumer;
import org.jdom.Verifier;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.util.io.URLUtil.URL_PATTERN;

/**
 * Shameless copy-pasta of {@link PlainTextSplitter} that also splits on
 * apostrophes since they are ubiquitous in Haskell identifiers.
 */
public class HaskellSpellcheckingSplitter extends BaseSplitter {

  private static final PlainTextSplitter INSTANCE = new PlainTextSplitter();

  public static PlainTextSplitter getInstance() {
    return INSTANCE;
  }

  @NonNls
  private static final
  Pattern SPLIT_PATTERN = Pattern.compile("(\\s|\b|')");

  @NonNls
  private static final Pattern MAIL =
    Pattern.compile("([\\p{L}0-9\\.\\-\\_\\+]+@([\\p{L}0-9\\-\\_]+(\\.)?)+(com|net|[a-z]{2})?)");

  @NonNls
  private static final Pattern UUID_PATTERN = Pattern.compile("[a-fA-F0-9]{8}(-[a-fA-F0-9]{4}){3}-[a-fA-F0-9]{12}");

  @Override
  public void split(@Nullable String text, @NotNull TextRange range, Consumer<TextRange> consumer) {
    if (StringUtil.isEmpty(text)) {
      return;
    }
    final Splitter ws = getTextSplitter();
    int from = range.getStartOffset();
    int till;

    try {
      Matcher matcher;
      final String substring = range.substring(text).replace('\b', '\n').replace('\f', '\n');
      if (Verifier.checkCharacterData(SPLIT_PATTERN.matcher(newBombedCharSequence(substring)).replaceAll("")) != null) {
        return;
      }
      matcher = SPLIT_PATTERN.matcher(newBombedCharSequence(text, range));

      while (true) {
        checkCancelled();
        List<TextRange> toCheck;
        TextRange wRange;
        String word;
        if (matcher.find()) {
          TextRange found = matcherRange(range, matcher);
          till = found.getStartOffset();
          if (badSize(from, till)) {
            from = found.getEndOffset();
            continue;
          }
          wRange = new TextRange(from, till);
          word = wRange.substring(text);
          from = found.getEndOffset();
        }
        else { // end hit or zero matches
          wRange = new TextRange(from, range.getEndOffset());
          word = wRange.substring(text);
        }
        if (word.contains("@")) {
          toCheck = excludeByPattern(text, wRange, MAIL, 0);
        }
        else if (word.contains("://")) {
          toCheck = excludeByPattern(text, wRange, URL_PATTERN, 0);
        }
        else if (word.contains("-")) {
          toCheck = excludeByPattern(text, wRange, UUID_PATTERN, 0);
        }
        else {
          toCheck = Collections.singletonList(wRange);
        }
        for (TextRange r : toCheck) {
          ws.split(text, r, consumer);
        }
        if (matcher.hitEnd()) break;
      }
    }
    catch (ProcessCanceledException ignored) {
    }
  }

  @NotNull
  protected Splitter getTextSplitter() {
    return TextSplitter.getInstance();
  }
}
