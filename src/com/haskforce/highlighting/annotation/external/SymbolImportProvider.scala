package com.haskforce.highlighting.annotation.external

trait SymbolImportProvider {
  def findImport(symbol: String): Seq[SymbolImportProvider.Result]
}

object SymbolImportProvider {
  final case class Result(importText: String, symbolText: String)
}