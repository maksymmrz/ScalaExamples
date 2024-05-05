package org.mmoroz.ehandling

import scala.util.control.NonFatal

enum EitherM[+E, +A]:
  case Left(e: E)
  case Right(a: A)

  def map[B](f: A => B): EitherM[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => EitherM[EE, B]): EitherM[EE, B] = this match
    case Right(a) => f(a)
    case Left(e)  => Left(e)

  def orElse[EE >: E, B >: A](default: => EitherM[EE, B]): EitherM[EE, B] =
    this match
      case Left(_) => default
      case right   => right

  def map2[EE >: E, B, C](
      that: EitherM[EE, B]
  )(f: (A, B) => C): EitherM[EE, C] =
    for
      a <- this
      b <- that
    yield f(a, b)

object EitherM:
  def catchNonFatal[A](expression: => A): EitherM[Throwable, A] =
    try Right(expression)
    catch case NonFatal(e) => Left(e)

  def sequence[E, A](seq: Seq[EitherM[E, A]]): EitherM[E, Seq[A]] =
    traverse(seq)(x => x)

  def traverse[E, A, B](
      seq: Seq[A]
  )(f: A => EitherM[E, B]): EitherM[E, Seq[B]] =
    seq.foldRight(Right[E, Seq[B]](Seq.empty[B]))((e, acc) =>
      f(e).map2(acc)(_ +: _)
    )

  def map2Both[E, A, B, C](
      first: EitherM[E, A],
      second: EitherM[E, B]
  )(f: (A, B) => C): EitherM[Seq[E], C] = (first, second) match
    case (Right(a), Right(b))          => Right(f(a, b))
    case (Right(_), Left(e))           => Left(Seq(e))
    case (Left(e), Right(_))           => Left(Seq(e))
    case (Left(firstE), Left(secondE)) => Left(Seq(firstE, secondE))

  def map2All[E, A, B, C](
      first: EitherM[Seq[E], A],
      second: EitherM[Seq[E], B]
  )(f: (A, B) => C): EitherM[Seq[E], C] = (first, second) match
    case (Right(a), Right(b))          => Right(f(a, b))
    case (Right(_), Left(e))           => Left(e)
    case (Left(e), Right(_))           => Left(e)
    case (Left(firstE), Left(secondE)) => Left(firstE ++ secondE)

  def traverseAll[E, A, B](
      seq: Seq[A]
  )(f: A => EitherM[Seq[E], B]): EitherM[Seq[E], Seq[B]] =
    seq.foldRight(Right[Seq[E], Seq[B]](Seq.empty[B]))((e, acc) =>
      map2All(f(e), acc)(_ +: _)
    )

  def sequenceAll[E, A](seq: Seq[EitherM[Seq[E], A]]): EitherM[Seq[E], Seq[A]] =
    traverseAll(seq)(x => x)
