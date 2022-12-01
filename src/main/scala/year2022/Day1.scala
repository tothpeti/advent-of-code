package year2022

import cats.effect.{IO, IOApp}
import fs2.Chunk
import fs2.io.file.{Files, Path}

object Day1 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day1/test.txt"
  // private val part1Path = "src/main/resources/year2022/day1/part1.txt"
  private val part2Path = "src/main/resources/year2022/day1/part2.txt"

  private def calculateSumOfCalories(calories: Chunk[String]): IO[Int] =
    IO {
      calories
        .map(_.toInt)
        .foldLeft(0)(_ + _)
    }

  private def getTopKhighestCalories(calories: List[Int], k: Int): IO[Int] =
    IO {
      calories
        .sortWith(_ > _)
        .take(k)
        .sum
    }

  private def processInput(path: String): IO[List[Int]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .split(_ == "")
      .evalMap(calculateSumOfCalories)
      .compile
      .toList

  override def run: IO[Unit] =
    // Part 1 solution
    /*
    for {
      list <- processInput(part1Path)
      _    <- IO(println(list.max))
    } yield ()
     */

    // Part 2 solution
    for {
      list <- processInput(part2Path)
      res  <- getTopKhighestCalories(list, 3)
      _    <- IO(println(res))
    } yield ()
}
