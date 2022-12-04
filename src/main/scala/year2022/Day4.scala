package year2022

import cats.implicits._
import cats.effect.{IO, IOApp}
import fs2.Pipe
import fs2.io.file.{Files, Path}

object Day4 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day4/test.txt"
  // private val part1Path = "src/main/resources/year2022/day4/part1.txt"
  private val part2Path = "src/main/resources/year2022/day4/part2.txt"

  final case class Elf(beginning: Int, end: Int)
  final case class Assignment(first: Elf, second: Elf)

  implicit class AssignmentOps(assignment: Assignment) {
    def intersect(): Boolean = {
      val firstElf  = assignment.first
      val secondElf = assignment.second

      val firstSection  = (firstElf.beginning to firstElf.end).toSet
      val secondSection = (secondElf.beginning to secondElf.end).toSet

      firstSection.subsetOf(secondSection) || secondSection.subsetOf(firstSection)
    }

    def overlap(): Boolean = {
      val firstElf  = assignment.first
      val secondElf = assignment.second

      val firstSection  = (firstElf.beginning to firstElf.end).toSet
      val secondSection = (secondElf.beginning to secondElf.end).toSet

      firstSection.intersect(secondSection).nonEmpty || secondSection.intersect(firstSection).nonEmpty
    }
  }

  private def createElf(data: String): Elf =
    data.split("-") match {
      case Array(beginning, end) =>
        Elf(beginning.toInt, end.toInt)
      case _ =>
        throw new IllegalStateException("Something went wrong! This should not happen...")
    }

  private def countIntersections(assignments: List[Assignment]): Int =
    assignments
      .count(_.intersect())

  private def countOverlaps(assignments: List[Assignment]): Int =
    assignments
      .count(_.overlap())

  private val convertInputToAssignment: Pipe[IO, String, Assignment] = stream =>
    stream
      .map(_.split(","))
      .map {
        case Array(section1, section2) =>
          Assignment(
            createElf(section1),
            createElf(section2)
          )
        case _ =>
          throw new IllegalStateException("Something went wrong! This should not happen...")
      }

  private def processInput(path: String): IO[List[Assignment]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .through(convertInputToAssignment)
      .compile
      .toList

  override def run: IO[Unit] =
    processInput(part2Path).flatMap { assignments =>
      (IO(countIntersections(assignments)), IO(countOverlaps(assignments))).parMapN { (solution1, solution2) =>
        (solution1, solution2)
      }
    }.flatTap { tuple =>
      IO(println(s"Part1: ${tuple._1}, Part2: ${tuple._2}"))
    }.void
}
