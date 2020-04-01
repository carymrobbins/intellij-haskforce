package com.haskforce.ui

import java.awt.GridBagConstraints._
import java.awt.{GridBagConstraints, Insets}

/**
 * Provides a functional wrapper for GridBagConstraints.
 */
sealed case class GC(
  // NOTE: These field names have to be different from GridBagConstraints since we can't override them.
  gridX: Int = RELATIVE,
  gridY: Int = RELATIVE,
  width: Int = 1,
  height: Int = 1,
  weightX: Double = 0,
  weightY: Double = 0,
  anchor_ : Int = CENTER,
  fill_ : Int = NONE,
  insets_ : Insets = new Insets(0, 0, 0, 0),
  padX: Int = 0,
  padY: Int = 0
) extends GridBagConstraints(
  gridX, gridY, width, height, weightX, weightY, anchor_, fill_, insets_, padX, padY
) {
  def grid(x: Int = gridX, y: Int = gridY) = copy(gridX = x, gridY = y)
  def weight(x: Double = weightX, y: Double = weightY) = copy(weightX = x, weightY = y)
  def pad(x: Int = padX, y: Int = padY) = copy(padX = x, padY = y)


  def width(w: Int) = copy(width = w)
  def height(h: Int): GC = copy(height = h)

  lazy val center = copy(anchor_ = CENTER)
  lazy val north = copy(anchor_ = NORTH)
  lazy val northEast = copy(anchor_ = NORTHEAST)
  lazy val east = copy(anchor_ = EAST)
  lazy val southEast = copy(anchor_ = SOUTHEAST)
  lazy val south = copy(anchor_ = SOUTH)
  lazy val southWest = copy(anchor_ = SOUTHWEST)
  lazy val west = copy(anchor_ = WEST)
  lazy val northWest = copy(anchor_ = NORTHWEST)
  lazy val firstLineStart = copy(anchor_ = FIRST_LINE_START)

  lazy val lastLineEnd = copy(anchor_ = LAST_LINE_END)

  lazy val fillBoth = copy(fill_ = BOTH)
  lazy val fillHorizontal = copy(fill_ = HORIZONTAL)
  lazy val fillVertical = copy(fill_ = VERTICAL)
  lazy val fillNone = copy(fill_ = NONE)
}

object GC {
  val default: GC = GC()
}
