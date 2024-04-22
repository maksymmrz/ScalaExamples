package org.mmoroz

import scala.annotation.tailrec

object ListsExercises extends App {

  val list = List(1, 2, 3, 4, 5, 6, 7)

  @tailrec
  def foldLeftTR[A, B](list: List[A], acc: B, f: (B, A) => B): B = list match
    case x :: xs => foldLeftTR(xs, f(acc, x), f)
    case Nil     => acc

  def foldRightTR[A, B](list: List[A], acc: B, f: (A, B) => B): B =
    val composedF =
      foldLeftTR(list, (b: B) => b, (fn, a) => (b: B) => fn(f(a, b)))
    composedF(acc)

  println(foldRightTR(list, 0, (a, b) => a + b))

  def append[T](first: List[T], second: List[T]): List[T] =
    @tailrec
    def appendLoop(a: List[T], b: List[T]): List[T] = a match
      case head :: tail => appendLoop(tail, head :: b)
      case Nil          => b
    appendLoop(first.reverse, second)

  def flatten[T](lists: List[List[T]]): List[T] =
    @tailrec
    def flattenLoop(matrix: List[List[T]], acc: List[T]): List[T] = matrix match
      case list :: tail => flattenLoop(tail, append(acc, list))
      case Nil          => acc
    flattenLoop(matrix = lists, acc = Nil)

  val lists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10))

  println(flatten(lists))

  def increment(integers: List[Int]): List[Int] =
    @tailrec
    def loop(list: List[Int], acc: List[Int]): List[Int] = list match
      case num :: tail => loop(tail, (num + 1) +: acc)
      case Nil         => acc
    loop(integers, Nil).reverse

  val integers = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  println(increment(integers))

  def mapList[A, B](list: List[A], f: A => B): List[B] =
    @tailrec
    def loop(list: List[A], f: A => B, acc: List[B]): List[B] = list match
      case head :: tail => loop(tail, f, f(head) +: acc)
      case Nil          => acc
    loop(list, f, Nil).reverse

  println(mapList(integers, _ * 2))

  def filterList[A](list: List[A], predicate: A => Boolean): List[A] =
    @tailrec
    def loop(list: List[A], p: A => Boolean, acc: List[A]): List[A] = list match
      case head :: tail =>
        if p(head) then loop(tail, p, head +: acc) else loop(tail, p, acc)
      case Nil => acc
    loop(list, predicate, Nil).reverse

  println(filterList(integers, _ < 7))

  def flatMapList[A, B](list: List[A], f: A => List[B]): List[B] = flatten(
    mapList(list, f)
  )
  println(flatMapList(integers, num => List.fill(3)(num * 2)))

  @tailrec
  def hasSubsequence[A](seq: List[A], sub: List[A]): Boolean = (seq, sub) match
    case (headSeq :: tailSeq, headSub :: tailSub) =>
      if headSeq == headSub then hasSubsequence(tailSeq, tailSub)
      else hasSubsequence(tailSeq, sub)
    case (Nil, _ :: _) => false
    case (_, Nil)      => true

  println(hasSubsequence(integers, List(2, 4, 8, 1)))
}
