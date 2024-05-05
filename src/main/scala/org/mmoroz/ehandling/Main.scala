package org.mmoroz.ehandling

object Main:

  @main def optionTask(): Unit =
    import OptionM.*

    val optList = List(SomeM(1), SomeM(2), SomeM(3), SomeM(4), SomeM(5), SomeM(6))
    val intList = List(1, 2, 3, 4, 5, 6, 7)
    println(optList.sequenceM)
    println(intList.traverseM(a => if a < 8 then SomeM(a) else NoneM))
  
  @main def eitherTask(): Unit =
    import EitherM.*

    val intList = List(1, 2, 3, 4, 5, 6, 7)
    val intValidList: List[EitherM[RuntimeException, Int]] =
      List(Right(1), Right(2), Right(3), Right(4), Right(5), Right(6), Right(7))
      
    val intInvalidList: List[EitherM[RuntimeException, Int]] =
      List(Right(1), Right(2), Right(3), Right(4), Left(new RuntimeException()), Right(6), Right(7))
    
    println(sequence(intValidList))
    println(sequence(intInvalidList))
    println(traverse(intList)(n => Right[Throwable, Int](n + 1)))