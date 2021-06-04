package com.haskforce.utils

/** NonEmptyList for Sets. */
final case class NonEmptySet[A] private(toSet: Set[A]) extends AnyVal {

  def iterator: Iterator[A] = toSet.iterator

  def map[B](f: A => B): NonEmptySet[B] = new NonEmptySet[B](toSet.map(f))

  def foreach[U](f: A => U): Unit = toSet.foreach[U](f)

  def append(ss: NonEmptySet[A]*): NonEmptySet[A] = {
    new NonEmptySet(ss.foldRight(toSet: Set[A])((s, acc) => acc ++ s.toSet))
  }
}

object NonEmptySet {

  def apply[A](a: A, as: A*): NonEmptySet[A] = new NonEmptySet(as.toSet + a)

  def fromSet[A](s: Set[A]): Option[NonEmptySet[A]] = {
    if (s.isEmpty) None else Some(new NonEmptySet(s))
  }

  def fromSets[A](ss: TraversableOnce[Set[A]]): Option[NonEmptySet[A]] = {
    fromSet(ss.foldLeft(Set.empty[A])((acc, s) => acc ++ s))
  }

  def fromNonEmptySets[A](ss: TraversableOnce[NonEmptySet[A]]): Option[NonEmptySet[A]] = {
    fromSets(ss.toIterator.map(_.toSet))
  }

  def fromTraversable[A](s: Traversable[A]): Option[NonEmptySet[A]] = {
    if (s.isEmpty) None else Some(new NonEmptySet(s.toSet))
  }

  def fromIterator[A](it: Iterator[A]): Option[NonEmptySet[A]] = {
    if (it.isEmpty) None else Some(new NonEmptySet(it.toSet))
  }

  def fromTraversables[A](ss: Traversable[Traversable[A]]): Option[NonEmptySet[A]] = {
    fromSets(ss.toStream.map(_.toSet))
  }

  implicit def jdom[A](
    implicit A: JDOMExternalizable[A]
  ): JDOMFieldExternalizable[NonEmptySet[A]] = {
    JDOMFieldExternalizable.iter[A].imap(
      it => fromIterator(it).getOrElse {
        throw new IllegalArgumentException("Unexpected empty set")
      },
      _.iterator
    )
  }
}
