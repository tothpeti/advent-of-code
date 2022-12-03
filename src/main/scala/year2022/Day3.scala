package year2022

import cats.implicits._
import cats.effect.{IO, IOApp}
import fs2.{Chunk, Pipe}
import fs2.io.file.{Files, Path}

object Day3 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day3/test.txt"
  // private val part1Path = "src/main/resources/year2022/day3/part1.txt"
  private val part2Path = "src/main/resources/year2022/day3/part2.txt"

  private lazy val lookupTable = generateAlphaWithPriority()

  private lazy val alphaSet = generateAlpha()

  private def generateAlphaWithPriority(): Map[Char, Int] = {
    val chars = ('a' to 'z') ++ ('A' to 'Z')
    chars.zip(LazyList.from(1)).toMap
  }

  private def generateAlpha(): Set[Char] = (('a' to 'z') ++ ('A' to 'Z')).toSet

  private val findCommonCharacterInTwoHalves: Pipe[IO, String, Char] = stream =>
    stream.map { line =>
      val firstHalf  = line.substring(0, line.length / 2).toSet
      val secondHalf = line.substring(line.length / 2).toSet

      firstHalf
        .intersect(secondHalf)
        .headOption
        .getOrElse(
          throw new IllegalStateException("Something went wrong! This should not happen...")
        )
    }

  private val findCommonCharacterInChunk: Pipe[IO, Chunk[String], Char] = stream =>
    stream.map { chunk =>
      chunk
        .map(_.toSet)
        .foldLeft(alphaSet)(_ intersect _)
        .headOption
        .getOrElse(
          throw new IllegalStateException("Something went wrong! This should not happen...")
        )
    }

  private def processInput(path: String): IO[Int] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .through(findCommonCharacterInTwoHalves)
      .map(lookupTable)
      .compile
      .toList
      .map(_.sum)

  private def processInput2(path: String): IO[Int] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .chunkN(3, allowFewer = false)
      .through(findCommonCharacterInChunk)
      .map(lookupTable)
      .compile
      .toList
      .map(_.sum)

  override def run: IO[Unit] =
    (processInput(part2Path), processInput2(part2Path)).parMapN { (solution1, solution2) =>
      (solution1, solution2)
    }.flatTap { tuple =>
      IO(println(s"Part1: ${tuple._1}, Part2: ${tuple._2}"))
    }.void
}
