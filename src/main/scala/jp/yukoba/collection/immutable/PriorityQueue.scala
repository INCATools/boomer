/*
https://github.com/yukoba/ScalaImmutablePriorityQueue
Copyright (c) 2016-2021, Yu Kobayashi
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

 * Neither the name of copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package jp.yukoba.collection.immutable

import scalaz.Maybe.Just
import scalaz.{FingerTree, Monoid, Reducer}

import scala.collection.AbstractSeq

class PriorityQueue[A] protected (val tree: FingerTree[A, A]) extends AbstractSeq[A] with Serializable {
  def enqueue(elem: A): PriorityQueue[A] = new PriorityQueue(tree :+ elem)
  def enqueue(elem: IterableOnce[A]): PriorityQueue[A] = new PriorityQueue(elem.iterator.foldLeft(tree)(_ :+ _))

  def :+(elem: A): PriorityQueue[A] = enqueue(elem)
  def +:(elem: A): PriorityQueue[A] = enqueue(elem)
  def ++(that: IterableOnce[A]): PriorityQueue[A] = enqueue(that)
  def ++:(that: IterableOnce[A]): PriorityQueue[A] = enqueue(that)

  def dequeue: (A, PriorityQueue[A]) = (head, tail)
  def dequeueOption: Option[(A, PriorityQueue[A])] = if (isEmpty) None else Some(dequeue)

  override def head: A = tree.split1(n => tree.measure == Just(n))._2
  def front: A = head

  override def tail: PriorityQueue[A] = {
    val t = tree.split1(n => tree.measure == Just(n))
    new PriorityQueue(t._1 <++> t._3)
  }

  override def isEmpty: Boolean = tree.isEmpty

  def toLazyList: LazyList[A] = LazyList.unfold(this)(t => if (t.isEmpty) None else Some(t.head, t.tail))

  override def iterator: Iterator[A] = toLazyList.iterator
  override def length: Int = tree.toStream.length
  override def apply(idx: Int): A = toLazyList(idx)

  override protected def className: String = "PriorityQueue"
}

object PriorityQueue {

  /** null is the smallest */
  private class MaxMonoid[A](ord: Ordering[A]) extends Monoid[A] {
    override def zero: A = null.asInstanceOf[A]

    override def append(f1: A, f2: => A): A =
      if (f1 == null) f2
      else if (f2 == null) f1
      else if (ord.lt(f1, f2)) f2
      else f1

  }

  private def MaxReducer[A](ord: Ordering[A]): Reducer[A, A] =
    Reducer.identityReducer[A](new MaxMonoid(ord))

  def empty[A](implicit ord: Ordering[A]): PriorityQueue[A] =
    new PriorityQueue(FingerTree.empty(MaxReducer[A](ord)))

  def apply[A](elems: A*)(implicit ord: Ordering[A]): PriorityQueue[A] = empty[A](ord).enqueue(elems)
}
