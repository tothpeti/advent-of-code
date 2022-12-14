package year2022

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

import scala.annotation.tailrec

object Day7 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day7/test.txt"
  // private val part1Path = "src/main/resources/year2022/day7/part1.txt"
  private val part2Path = "src/main/resources/year2022/day7/part2.txt"

  private def buildStructure(fileEcosystem: List[String]): Map[String, Int] = {
    @tailrec
    def helper(
      remaining: List[String],
      stack: List[String],
      sizes: Map[String, Int]
    ): Map[String, Int] =
      remaining match {
        case Nil => sizes
        case head :: tail =>
          head.split(" ") match {
            case Array(_, _, "/")    => helper(tail, List.empty, sizes)
            case Array(_, _, "..")   => helper(tail, stack.tail, sizes)
            case Array(_, _, folder) => helper(tail, folder :: stack, sizes)
            case Array(size, _) if size.forall(Character.isDigit) =>
              val sizesUpdated = (0 to stack.size).foldLeft(sizes) { (seed, elem) =>
                val path = "/" + stack.takeRight(elem).mkString("/")
                seed.updated(path, seed.getOrElse(path, 0) + size.toInt)
              }
              helper(tail, stack, sizesUpdated)
            case _ =>
              helper(tail, stack, sizes)
          }
      }

    helper(fileEcosystem, List.empty, Map.empty)
  }

  private def solutionPart1(structure: Map[String, Int]): Int =
    structure.values
      .filter(_ <= 100000)
      .sum

  private def solutionPart2(structure: Map[String, Int]): Int = {
    val unused = 70000000 - structure("/")
    val need   = 30000000 - unused

    structure.values
      .filter(_ >= need)
      .min
  }

  private def processInput(path: String): IO[Map[String, Int]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .compile
      .toList
      .map(buildStructure)

  override def run: IO[Unit] =
    for {
      res <- processInput(part2Path)
      _   <- IO(println(s"${solutionPart1(res)}"))
      _   <- IO(println(s"${solutionPart2(res)}"))
    } yield ()
}
