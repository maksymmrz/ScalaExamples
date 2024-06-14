package org.mmoroz.nonstrictness

import org.mmoroz.nonstrictness.LazyListM.empty
import LazyListM.*

import scala.annotation.tailrec

enum LazyListM[+A]:
  case Cons(head: () => A, tail: () => LazyListM[A])
  case Empty

  def headOpt: Option[A] =
    foldRight(Option.empty[A])((e, _) => Some(e))

  def getTail: LazyListM[A] = this match
    case Cons(_, t) => t()
    case _          => Empty

  def foldLeft[B](zero: B)(f: (=> B, A) => B): B = this match
    case Cons(h, t) => t().foldLeft(f(zero, h()))(f)
    case Empty      => zero

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case Empty      => acc

  def map[B](f: A => B): LazyListM[B] =
    foldRight(empty[B])((e, acc) => Cons(() => f(e), () => acc))

  def filter(p: A => Boolean): LazyListM[A] =
    foldRight(empty[A])((e, acc) =>
      if p(e) then Cons(() => e, () => acc) else acc
    )

  def append[AA >: A](that: LazyListM[AA]): LazyListM[AA] =
    foldRight(that)((e, acc) => cons(e, acc))

  def flatMap[B](f: A => LazyListM[B]): LazyListM[B] =
    foldRight(empty[B])((e, acc) => f(e).append(acc))

  def exists(p: A => Boolean): Boolean = foldLeft(false)(_ || p(_))

  def forAll(p: A => Boolean): Boolean = foldLeft(true)(_ && p(_))

  def reverse: LazyListM[A] =
    foldRight(LazyListM.empty[A])((e, list) => Cons(() => e, () => list))

  def drop(n: Int): LazyListM[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def take(n: Int): LazyListM[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), Empty)
    case _                    => Empty

  def takeWhile(p: A => Boolean): LazyListM[A] =
    foldRight(empty[A])((e, acc) => if p(e) then cons(e, acc) else Empty)

  def toList: List[A] = foldRight(List.empty[A])(_ +: _)

  def print(comment: String): Unit = println(
    s"$comment - ${toList.mkString("[", ",", "]")}"
  )

  def mapViaUnfold[B](f: A => B): LazyListM[B] =
    unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case _          => None

  def takeViaUnfold(n: Int): LazyListM[A] =
    unfold((this, n)):
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _                        => None

  def takeWhileViaUnfold(p: A => Boolean): LazyListM[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None

  def zipWith[B](that: LazyListM[B]): LazyListM[(A, B)] =
    unfold((this, that)):
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        Some((head1(), head2()), (tail1(), tail2()))
      case _ => None

  def zipAll[B](that: LazyListM[B]): LazyListM[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        Some((Some(head1()), Some(head2())), (tail1(), tail2()))
      case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty))
      case (_, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (_, _)          => None

  def startsWith[AA >: A](that: LazyListM[AA]): Boolean =
    zipWith(that).foldRight(true):
      case ((a, b), true) => a == b
      case _              => false

  def tails: LazyListM[LazyListM[A]] =
    unfold(this):
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _          => None

  def hasSubsequence[AA >: A](that: LazyListM[AA]): Boolean =
    @tailrec
    def loop(sequence: LazyListM[A], subsequence: LazyListM[AA]): Boolean =
      (sequence, subsequence) match
        case (Empty, Cons(hs, ts))                     => false
        case (_, Empty)                                => true
        case (Cons(h, t), Cons(hs, ts)) if h() == hs() => loop(t(), ts())
        case (Cons(h, t), subs)                        => loop(t(), subs)
    loop(this, that)

object LazyListM:

  def apply[A](xs: A*): LazyListM[A] =
    if xs.isEmpty then empty[A]
    else cons(xs.head, apply(xs.tail*))

  def cons[A](head: => A, tail: => LazyListM[A]): LazyListM[A] =
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)

  def empty[A]: LazyListM[A] = Empty

  def continually[A](a: A): LazyListM[A] = cons(a, continually(a))

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): LazyListM[A] = f(s) match
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty[A]
