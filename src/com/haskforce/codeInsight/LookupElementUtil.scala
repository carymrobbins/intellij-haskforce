package com.haskforce.codeInsight

import java.util
import java.util.stream.Collectors

import com.haskforce.HaskellIcons
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}

object LookupElementUtil {
  def create(name: String, module: String, typ: String): LookupElement = {
    LookupElementBuilder.create(name).withIcon(HaskellIcons.FILE)
      .withTailText(s" ($module)", true)
      .withTypeText(typ)
  }

  def fromString(s: String): LookupElement = {
    LookupElementBuilder.create(s).withIcon(HaskellIcons.FILE)
  }

  def fromStrings(ss: Array[String]): util.List[LookupElement] = {
    util.Arrays.asList(ss.map(fromString): _*)
  }

  def fromStrings(ss: util.Collection[String]): util.List[LookupElement] = {
    ss.stream().map[LookupElement](fromString).collect(Collectors.toList())
  }

  def fromBrowseItem(b: BrowseItem): LookupElement = {
    create(b.name, b.module, b.typ)
  }
}
