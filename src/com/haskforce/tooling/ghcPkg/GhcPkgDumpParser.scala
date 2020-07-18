package com.haskforce.tooling.ghcPkg

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util
import java.util.regex.Pattern

object GhcPkgDumpParser {

  def parse(in: InputStream): Iterator[Pkg] = {
    val r = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))
    val it = Iterator.continually(r.readLine()).takeWhile(_ != null)
    Iterator
      .continually(parseChunk(it).flatMap(parsePkg))
      .takeWhile(_.isDefined)
      .map(_.get)
  }

  private def parseChunk(it: Iterator[String]): Option[List[String]] = {
    val res = it.takeWhile(_ != "---").dropWhile(_ == "---").toList
    if (res.isEmpty) None else Some(res)
  }

  private def parsePkg(chunk: List[String]): Option[Pkg] = {
    for {
      name <- parseName(chunk)
      version <- parseVersion(chunk)
      exposedModules = parseExposedModules(chunk)
    } yield Pkg(
      name,
      version,
      exposedModules
    )
  }

  private val spacesRegex = Pattern.compile("\\s+")

  private def words(s: String): Array[String] = spacesRegex.split(s.trim)

  private def parseField(key: String, chunk: List[String]): Option[String] = {
    for {
      line <- chunk.find(_.startsWith(s"$key:"))
      ss = words(line)
      value <- ss.lift(1).filter(_.nonEmpty)
    } yield value
  }

  private def parseName(chunk: List[String]): Option[String] = {
    parseField("name", chunk)
  }

  private def parseVersion(chunk: List[String]): Option[String] = {
    parseField("version", chunk)
  }

  private def parseExposedModules(chunk: List[String]): util.Set[String] = {
    val it = chunk.iterator.dropWhile(s => !s.startsWith("exposed-modules:"))
    if (!it.hasNext) return util.Collections.emptySet()
    val wordIt = words(it.next()).iterator.drop(1)
    val valuesIt = wordIt ++ it.takeWhile(_.startsWith(" "))
    val res = new util.HashSet[String]()
    valuesIt.foreach { s => words(s).foreach { w => res.add(w) } }
    res
  }
}
