package org.mmoroz.nonstrictness

import LazyListM._

object NonStrictnessMain {

  @main def lazyListRun(): Unit =

    LazyListM(1, 2, 3, 4, 5, 6, 7, 8, 9, 111, 222, 333)
      .map(_ + 10)
      .flatMap(e => LazyListM(e, e))
      .filter(_ % 2 == 0)
      .print

    val intLL = LazyListM(1, 2, 3, 4, 5, 6, 7, 8, 9)

    intLL.take(5).print("take")
    intLL.drop(5).print("drop")
    intLL.takeWhile(_ != 5).print("takeWhile")

    val sum =
      LazyListM
        .continually(1)
        .take(100)
        .mapViaUnfold(_ + 1)
        .foldRight(0)(_ + _)
    println(sum)

    fibsL.take(10).print("fibs loop")
    fibsU.take(10).print("fibs unfold")

    intLL.mapViaUnfold(_ * 10).print("mapViaUnfold")
    intLL.takeViaUnfold(5).print("takeViaUnfold")
    intLL.takeWhileViaUnfold(_ != 5).print("takeWhileViaUnfold")

    intLL.map(_ + 10).take(7).zipWith(intLL).print("zipWith")
    intLL.take(7).zipAll(intLL).print("zipAll")

    println("startsWith - " + intLL.startsWith(intLL.drop(5)))

    val tailsS =
      intLL
        .take(5)
        .tails
        .toList
        .map(_.toList.mkString("[", ",", "]"))
        .mkString("[", ",", "]")
    println(s"tails - $tailsS")
    
    println(s"hasSubsequence - ${intLL.hasSubsequence(LazyListM(5, 7, 8))}")

  def fibsU: LazyListM[Int] = unfold((0, 1)) { case (curr, next) =>
    Some(curr, (next, curr + next))
  }

  def fibsL: LazyListM[Int] =
    def loop(current: Int, next: Int): LazyListM[Int] =
      cons(current, loop(next, current + next))
    loop(0, 1)

}
