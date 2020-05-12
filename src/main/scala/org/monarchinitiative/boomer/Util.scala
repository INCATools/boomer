package org.monarchinitiative.boomer

import scala.annotation.tailrec
import scala.collection.Searching.InsertionPoint

object Util {

  implicit final class BisectSearchOp[A](val self: Seq[A]) extends AnyVal {

    /**
     * Find an index in the sequence where the result of the given predicate function
     * changes from true to false. If there is more than one such index, the result
     * will not necessarily be useful.
     * For example, `List("a", "aa", "aaa").bisect(_.length < 3)` returns `InsertionPoint(2)`
     */
    def bisect(predicate: A => Boolean): InsertionPoint = bisect(0, self.size)(predicate)

    /**
     * Find an index within the specified range of the sequence where the result of the given predicate function
     * changes from true to false. If there is more than one such index, the result
     * will not necessarily be useful.
     */
    @tailrec
    def bisect(from: Int, to: Int)(predicate: A => Boolean): InsertionPoint = {
      if (from < 0) bisect(0, to)(predicate)
      else if (to > self.size) bisect(from, self.size)(predicate)
      else if (to <= from) InsertionPoint(from)
      else {
        val index = from + (to - from - 1) / 2
        if (predicate(self.apply(index))) bisect(index + 1, to)(predicate)
        else bisect(from, index)(predicate)
      }
    }

  }

  def groupByValueWindows[A](data: List[A], windowCount: Int, value: A => Double): List[List[A]] = {
    def histo(bounds: List[Double], data: List[A]): List[List[A]] =
      bounds match {
        case Nil      => Nil
        case _ :: Nil => List(data)
        case h :: t   =>
          val (l, r) = data.partition(elem => value(elem) < h)
          l :: histo(t, r)
      }

    val bounds = (for {
      i <- 1 to windowCount
    } yield {
      (1.0 / windowCount) * i
    }).toList
    histo(bounds, data)
  }

}
