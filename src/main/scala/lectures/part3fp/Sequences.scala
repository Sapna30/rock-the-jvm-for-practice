package lectures.part3fp

import java.util

import scala.util.Random

object Sequences extends App {
  //Seq
  val aSequence = Seq(1, 3, 4, 2)
  println(aSequence)
  println(aSequence.reverse)
  println(aSequence(2))
  println(aSequence ++ Seq(7, 6, 5))
  println(aSequence.sorted)

  //Ranges
  val aRange: Seq[Int] = 1 to 10
  aRange.foreach(println)

  (1 to 10).map(x => println("Hello"))

  //lists
  val aList = List(1, 2, 3)
  val prepended = 42 :: aList
  println(prepended)

  val prepended_appended = 42 +: aList :+ 89
  println(prepended_appended)

  val apples = List.fill(5)("apple")
  println(apples)
  println(apples.mkString("-|-"))

  //Arrays
  val numbers = Array(1, 2, 3, 4)
  val threeElelements = Array.ofDim[Int](3)
  threeElelements.foreach(println)

  //mutation
  numbers(2) = 0 //syntax sugar for numbers.update(2,0)
  println(numbers.mkString(" "))

  //arrays and sequence
  val numberSeq: Seq[Int] = numbers //implicit conversion
  println(numberSeq)

  //Vectors
  val maxRuns = 1000
  val maxCapacity = 100000

  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime()
      collection.updated(r.nextInt(maxCapacity), r.nextInt())
      System.nanoTime() - currentTime
    }
    times.sum * 1.0 / maxRuns //Average time
  }

  val numberList = (1 to maxCapacity).toList
  val vectorList = (1 to maxCapacity).toVector
  //Keep reference to tail
  //updating an element in the middle takes long
  println(getWriteTime(numberList))
  // depth of trees is small
  // needs to replace an entire 32-element chunk
  println(getWriteTime(vectorList))

}
