package year2022

import cats.implicits._
import cats.effect.{IO, IOApp}
import fs2.Pipe
import fs2.io.file.{Files, Path}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Day5 extends IOApp.Simple {
  // private val testPath = "src/main/resources/year2022/day5/test.txt"
  private val part1Path = "src/main/resources/year2022/day5/part1.txt"
  private val part2Path = "src/main/resources/year2022/day5/part2.txt"

  implicit class ListOps[A](list: List[A]) {
    def pop(amount: Int): (List[A], List[A]) = {
      @tailrec
      def helper(remaining: List[A], stack: List[A], counter: Int): (List[A], List[A]) =
        if (counter == 0) (stack, remaining)
        else {
          remaining match {
            case Nil          => (stack, remaining)
            case head :: tail => helper(tail, head :: stack, counter - 1)
          }
        }

      helper(remaining = list, stack = List.empty, counter = amount)
    }
  }

  private final case class Move(quantity: Int, from: Int, to: Int)

  /* For Test */
  /*
  private val containersMap = SortedMap(
    1 -> List("N", "Z"),
    2 -> List("D", "C", "M"),
    3 -> List("P")
  )
   */

  /* For Part1 & Part2 */
  private val containersMap = SortedMap(
    1 -> List("W", "L", "S"),
    2 -> List("Q", "N", "T", "J"),
    3 -> List("J", "F", "H", "C", "S"),
    4 -> List("B", "G", "N", "W", "M", "R", "T"),
    5 -> List("B", "Q", "H", "D", "S", "L", "R", "T"),
    6 -> List("L", "R", "H", "F", "V", "B", "J", "M"),
    7 -> List("M", "J", "N", "R", "W", "D"),
    8 -> List("J", "D", "N", "H", "F", "T", "Z", "B"),
    9 -> List("T", "F", "B", "N", "Q", "L", "H")
  )

  private val convertInputToMove: Pipe[IO, Array[String], Move] = stream =>
    stream.map {
      case Array(quantity, from, to) =>
        Move(quantity = quantity.toInt, from = from.toInt, to = to.toInt)
      case _ =>
        throw new IllegalStateException("Something went wrong! This should not happen...")
    }

  private def retrieveTopContainers(containers: Map[Int, List[String]]): String =
    containers.values.map { container =>
      container.headOption
        .getOrElse(
          throw new IllegalStateException("Something went wrong! This should not happen...")
        )
    }.mkString("")

  private def rearrange(moves: List[Move], sort: Boolean): Map[Int, List[String]] =
    moves
      .foldLeft(containersMap) { (seed, current) =>
        val (poppedContainers, remaining) = seed(current.from).pop(current.quantity)
        val updatedContainers =
          if (sort) poppedContainers.reverse ::: seed(current.to)
          else poppedContainers ::: seed(current.to)

        seed
          .updated(current.from, remaining)
          .updated(current.to, updatedContainers)
      }

  private def process(path: String): IO[List[Move]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .map(_.split(" "))
      .map(_.filter(_.exists(_.isDigit)))
      .through(convertInputToMove)
      .compile
      .toList

  override def run: IO[Unit] =
    for {
      list <- process(part1Path)
                .map(moves => rearrange(moves, sort = false))
                .map(retrieveTopContainers)
      _ <- IO(println(list))
      list2 <- process(part2Path)
                 .map(moves => rearrange(moves, sort = true))
                 .map(retrieveTopContainers)
      _ <- IO(println(list2))
    } yield ()

}
