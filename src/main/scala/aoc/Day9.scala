package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.Searching.Found

object Day9 extends IOApp.Simple {
  type Input = Long
  type Count = Int
  type Index = Int

  val PreambleSize = 25

  def checkNumbers(numbers: List[Input]) = {
    def incrementCount(count: Option[Count]) = count.fold(1)(_ + 1).some

    @tailrec
    def loop(
        previousNums: Queue[Input],
        sumCounts: Map[Input, Count],
        nums: List[(Input, Index)]
    ): Option[Input] = {
      nums match {
        case Nil =>
          None
        case (x, i) :: xs if i < PreambleSize =>
          val newSumCounts =
            previousNums.foldLeft(sumCounts) { case (acc, n) =>
              acc.updatedWith(n + x)(incrementCount)
            }
          loop(previousNums.enqueue(x), newSumCounts, xs)
        case (x, _) :: _ if sumCounts.getOrElse(x, 0) == 0 =>
          Some(x)
        case (x, _) :: xs =>
          val (oldest, queue) = previousNums.dequeue
          val newSumCounts =
            queue.foldLeft(sumCounts) { case (acc, n) =>
              acc
                .updatedWith(n + x)(incrementCount)
                .updatedWith(n + oldest)(_.map(_ - 1))
            }
          loop(queue.enqueue(x), newSumCounts, xs)
      }
    }
    loop(Queue.empty, Map.empty, numbers.zipWithIndex)
  }

  def findContiguousSet(wantedSum: Input, nums: Array[Input]): Option[Input] = {
    val prefixSums = Array.ofDim[Input](nums.length + 1)
    for ((n, i) <- nums.zipWithIndex) {
      prefixSums(i + 1) = prefixSums(i) + n
    }

    @tailrec
    def loop(startIdx: Int): Option[Input] = {
      val startSum = prefixSums(startIdx)
      prefixSums
        .search(wantedSum + startSum, startIdx + 1, prefixSums.length) match {
        case Found(endIdx) =>
          val contiguousSet = nums.slice(startIdx, endIdx)
          Some(contiguousSet.min + contiguousSet.max)
        case _ if startIdx >= prefixSums.length - 1 =>
          None
        case _ =>
          loop(startIdx + 1)
      }
    }

    loop(0)
  }

  def answer1(input: List[String]) =
    input.traverse(_.toLongOption).flatMap(checkNumbers)

  def answer2(input: List[String]) =
    for {
      nums <- input.traverse(_.toLongOption)
      sum  <- checkNumbers(nums)
      ans  <- findContiguousSet(sum, nums.toArray)
    } yield ans

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day9.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
