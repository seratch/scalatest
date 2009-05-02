/*
 * Ordinal.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.scalatest

import java.util.Arrays

// TODO: equals and hashCode
final class Ordinal private (val runStamp: Int, private val stamps: Array[Int]) extends Ordered[Ordinal] {

  def this(runStamp: Int) = this(runStamp, Array(0))

  def next: Ordinal = {
    val newArray = new Array[Int](stamps.length) // Can't seem to clone
    val zipped = stamps.zipWithIndex
    for ((num, idx) <- zipped)
      newArray(idx) = num
    newArray(stamps.length - 1) += 1
    new Ordinal(runStamp, newArray)
  }

  /*
the first one is the ordinal for the new suite, the next one
is the ordinal for this suite. The reason is the first one is less
than the second one. All the new suite stuff happens "before" whatever
comes next in the old suite. Has this algo:
[scalatest] List(99, 0, 1)      ord
[scalatest] List(99, 0, 1, 0)   ordForNewSuite
[scalatest] List(99, 0, 2)      ordForOldSuite
  */
  def nextForNewSuite: (Ordinal, Ordinal) = {
    val newArrayForNewSuite = new Array[Int](stamps.length + 1)
    val newArrayForOldSuite = new Array[Int](stamps.length)
    val zipped = stamps.zipWithIndex
    for ((num, idx) <- zipped) {
      newArrayForNewSuite(idx) = num
      newArrayForOldSuite(idx) = num
    }
    newArrayForOldSuite(stamps.length - 1) += 1
    (new Ordinal(runStamp, newArrayForNewSuite), new Ordinal(runStamp, newArrayForOldSuite))
  }

  def toList: List[Int] = runStamp :: stamps.toList

  def compare(that: Ordinal) = {
    val runStampDiff = this.runStamp - that.runStamp
    if (runStampDiff == 0) {
      val shorterLength =
        if (this.stamps.length < that.stamps.length)
          this.stamps.length
        else
          that.stamps.length
      var i = 0
      var diff = 0
      while (diff == 0 && i < shorterLength) {
        diff = this.stamps(i) - that.stamps(i)
        i += 1
      }
      // If they were equal all the way to the shorterLength, the longest array
      // one is the greater ordinal. This is because the newSuite stuff happens
      // before the next thing that happens in the old suite.
      if (diff != 0) diff
      else this.stamps.length - that.stamps.length
    }
    else runStampDiff
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Ordinal =>
        runStamp == that.runStamp &&
        (stamps deepEquals that.stamps)
      case _ => false
    }

  override def hashCode: Int =
    41 * (
      41 + runStamp
    ) + Arrays.hashCode(stamps)
}
