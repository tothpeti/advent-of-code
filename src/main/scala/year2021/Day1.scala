package year2021

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

object Day1 extends IOApp.Simple {

  // private val testPath  = "src/main/resources/year2021/day1/test.txt"
  private val part1Path = "src/main/resources/year2021/day1/part1.txt"
  private val part2Path = "src/main/resources/year2021/day1/part2.txt"

  private def countMeasurementIncrease(measurements: List[Int]): Int =
    measurements
      .sliding(2)
      .foldLeft(0) { (seed, current) =>
        if (current.head < current.last) seed + 1
        else seed
      }

  private def calculateSumOfMeasurements(measurements: List[Int]): List[Int] =
    measurements
      .sliding(3)
      .map(_.sum)
      .toList

  private def processInput(path: String): IO[List[Int]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .map(_.toInt)
      .compile
      .toList

  override def run: IO[Unit] =
    for {
      res <- processInput(part1Path)
               .map(countMeasurementIncrease)
      _ <- IO(println(res))
      res2 <- processInput(part2Path)
                .map(calculateSumOfMeasurements)
                .map(countMeasurementIncrease)
      _ <- IO(println(res2))
    } yield ()
}
