package org.mmoroz
package org.mmoroz

import scala.annotation.tailrec

object Folds extends App {

  val list = List(1,2,3,4,5,6,7)

  @tailrec
  def foldLeftTR[A, B](list: List[A], acc: B, f: (B, A) => B): B = list match
    case x :: xs => foldLeftTR(xs, f(acc, x), f)
    case Nil => acc

  def foldRightTR[A, B](list: List[A], acc: B, f: (A, B) => B): B =
    val composedF = foldLeftTR(list, (b: B) => b, (fn, a) => (b: B) => fn(f(a, b)))
    composedF(acc)

  foldRightTR(list, 0, (a,b) =>
    println(a)
    a + b
  )
}
