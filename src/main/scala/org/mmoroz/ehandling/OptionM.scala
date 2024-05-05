package org.mmoroz.ehandling

enum OptionM[+A]:
  case SomeM(v: A)
  case NoneM

  def map[B](f: A => B): OptionM[B] = this match
    case SomeM(v) => SomeM(f(v))
    case NoneM    => NoneM

  def filter(p: A => Boolean): OptionM[A] = this match
    case SomeM(v) => if p(v) then SomeM(v) else NoneM
    case NoneM    => NoneM

  def flatMap[B](f: A => OptionM[B]): OptionM[B] = this match
    case SomeM(v) => f(v)
    case NoneM    => NoneM

  def getOrElse[B >: A](default: => B): B = this match
    case SomeM(v) => v
    case NoneM    => default

  def orElse[B >: A](op: => OptionM[B]): OptionM[B] = this match
    case SomeM(v) => SomeM(v)
    case NoneM    => op

object OptionM:

  extension [A](list: List[OptionM[A]])
    def sequenceM: OptionM[List[A]] =
      val resOpt =
        list.reverse
          .foldLeft(SomeM(List.empty[A])) {
            case (SomeM(acc), SomeM(elem)) => SomeM(elem +: acc)
            case (_, _)                    => NoneM
          }
      resOpt.filter(_.nonEmpty)

  extension [A](list: List[A])
    def traverseM[B](f: A => OptionM[B]): OptionM[List[B]] =
      list.reverse
        .foldLeft(SomeM(List.empty[B])) {
          case (SomeM(acc), elem) =>
            f(elem) match
              case SomeM(res) => SomeM(res +: acc)
              case NoneM => NoneM
          case _ => NoneM
        }

  


