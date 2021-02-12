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
