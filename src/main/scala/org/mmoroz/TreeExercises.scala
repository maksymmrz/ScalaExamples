package org.mmoroz

import scala.util.Random

enum Tree[+A]:
  case Leaf(v: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:
  def generate[A](source: => A, depth: Int): Tree[A] =
    if (depth > 1)
      Branch(
        left = generate(source, depth - 1),
        right = generate(source, depth - 1)
      )
    else Leaf(source)

  extension [A](t: Tree[A]) def size: Int = t match
    case Leaf(v) => 1
    case Branch(l, r) => l.size + r.size

  extension [A](t: Tree[A]) def depth: Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + Math.max(l.depth, r.depth)

  extension [A](t: Tree[A]) def flatten: Vector[A] = t match
    case Leaf(v) => Vector(v)
    case Branch(l, r) => l.flatten ++ r.flatten

  extension [A, B](t: Tree[A]) def map(f: A => B): Tree[B] = t match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  extension [A](t: Tree[A]) def fold(f: (A, A) => A): A = t match
    case Leaf(v) => v
    case Branch(l, r) => f(l.fold(f), r.fold(f))

  extension (t: Tree[Long]) def maximum: Long = t match
    case Leaf(v) => v
    case Branch(l, r) => Math.max(l.maximum, r.maximum)

  extension (t: Tree[Long]) def firstPositive[A]: Long = t match
    case Leaf(v) => v
    case Branch(l, r) =>
      val lPos = l.firstPositive
      if lPos > 0 then lPos
      else r.firstPositive


object TreeExercises extends App {
  import Tree.*
  val longTree = generate(Random.nextLong(10000000), 9)

  println(s"tree size: ${longTree.size}")
  println(longTree.depth)

  println(longTree.flatten)
  println(longTree.map(_ + 1).flatten)

  println(longTree.flatten.sum)
  println(longTree.fold(_ + _))




}
