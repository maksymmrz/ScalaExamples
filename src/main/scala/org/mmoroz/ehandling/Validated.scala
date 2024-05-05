package org.mmoroz.ehandling

import org.mmoroz.ehandling.EitherM.*

enum Validated[+E, +A]:
  case Valid(a: A)
  case Invalid(errors: Seq[E])

  def toEitherM: EitherM[Seq[E], A] = this match
    case Valid(a)        => Right(a)
    case Invalid(errors) => Left(errors)

  def map[B](f: A => B): Validated[E, B] = this match
    case Valid(a)        => Valid(f(a))
    case Invalid(errors) => Invalid(errors)

  def map2[EE >: E, B, C](that: Validated[EE, B])(
      f: (A, B) => C
  ): Validated[EE, C] = (this, that) match
    case (Valid(a), Valid(b))       => Valid(f(a, b))
    case (Valid(_), Invalid(e))     => Invalid(e)
    case (Invalid(e), Valid(_))     => Invalid(e)
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)

object Validated:

  def fromEither[E, A](either: EitherM[Seq[E], A]): Validated[E, A] =
    either match
      case Right(a) => Valid(a)
      case Left(e)  => Invalid(e)

  def traverse[E, A, B](
      seq: Seq[A]
  )(f: A => Validated[E, B]): Validated[E, Seq[B]] =
    seq.foldRight(Valid(Seq.empty[B]): Validated[E, Seq[B]])((a, acc) =>
      f(a).map2(acc)(_ +: _)
    )

  def sequence[E, A](seq: Seq[Validated[E, A]]): Validated[E, Seq[A]] =
    traverse(seq)(identity)
