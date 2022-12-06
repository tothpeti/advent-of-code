package year2021

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

object Day3 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2021/day3/test.txt"
  private val part1Path = "src/main/resources/year2021/day3/part1.txt"
  // private val part2Path = "src/main/resources/year2021/day3/part2.txt"

  private def retrieveLeastCommonBit(path: String) =
    Files[IO]
      .readUtf8Lines(Path(path))
      .compile
      .toList
      .map(_.transpose)
      .map(_.map(_.groupBy(identity).view.mapValues(_.size).toList))
      .map(_.map(l => l.minBy(_._2)))
      .map(_.map(_._1).mkString(""))

  private def retrieveMostCommonBit(path: String) =
    Files[IO]
      .readUtf8Lines(Path(path))
      .compile
      .toList
      .map(_.transpose)
      .map(_.map(_.groupBy(identity).view.mapValues(_.size).toList))
      .map(_.map(l => l.maxBy(_._2)))
      .map(_.map(_._1).mkString(""))

  override def run: IO[Unit] =
    for {
      leastCommonBits <- retrieveLeastCommonBit(part1Path)
      mostCommonBits  <- retrieveMostCommonBit(part1Path)
      least            = Integer.parseInt(leastCommonBits, 2)
      most             = Integer.parseInt(mostCommonBits, 2)

      _ <- IO(println(least * most))
    } yield ()
}
