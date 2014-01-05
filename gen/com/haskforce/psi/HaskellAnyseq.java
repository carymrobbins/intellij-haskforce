// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellAnyseq extends PsiElement {

  @NotNull
  List<HaskellQconid> getQconidList();

  @NotNull
  List<HaskellQconsym> getQconsymList();

  @NotNull
  List<HaskellQvarid> getQvaridList();

  @NotNull
  List<HaskellQvarsym> getQvarsymList();

  @NotNull
  List<HaskellReservedid> getReservedidList();

  @NotNull
  List<HaskellReservedop> getReservedopList();

}
