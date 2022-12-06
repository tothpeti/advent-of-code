package year2022

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

object Day6 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day6/test.txt"
  // private val part1Path = "src/main/resources/year2022/day6/part1.txt"
  private val part2Path = "src/main/resources/year2022/day6/part2.txt"

  private def detector(str: String, arr: Array[Char], size: Int): Int =
    str.indexOf(
      arr
        .sliding(size)
        .map(a => (a, a.toSet))
        .find(_._2.size == size)
        .map(_._1.mkString)
        .getOrElse(
          throw new IllegalStateException("Something went wrong! This should not happen...")
        )
    ) + size

  private def processInput(path: String, windowSize: Int): IO[List[Int]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .map(x => detector(x, x.toCharArray, windowSize))
      .debug(o => s"$o")
      .compile
      .toList

  override def run: IO[Unit] = processInput(part2Path, 14).void
}
