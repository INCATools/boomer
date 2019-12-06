package org.monarchinitiative.boomer

import scala.annotation.tailrec
import scala.collection.Searching.InsertionPoint

object Util {

  implicit final class BisectSearchOp[A](val self: Seq[A]) extends AnyVal {

    final def bisect(predicate: A => Boolean): InsertionPoint = bisect(0, self.size)(predicate)

    @tailrec
    final def bisect(from: Int, to: Int)(predicate: A => Boolean): InsertionPoint = {
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

}
