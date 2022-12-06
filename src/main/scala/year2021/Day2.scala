package year2021

import cats.effect.{IO, IOApp}
import fs2.Pipe
import fs2.io.file.{Files, Path}

object Day2 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2021/day2/test.txt"
  // private val part1Path = "src/main/resources/year2021/day2/part1.txt"
  private val part2Path = "src/main/resources/year2021/day2/part2.txt"

  private final case class Instruction(direction: String, amount: Int)

  type Horizontal = Int
  type Depth      = Int
  type Aim        = Int

  private val convertInputToInstruction: Pipe[IO, String, Instruction] = stream =>
    stream
      .map(_.split(" "))
      .map {
        case Array(direction, amount) =>
          Instruction(direction, amount.toInt)
        case _ =>
          throw new IllegalStateException("Something went wrong! This should not happen...")
      }

  private def calculatePosition(instructions: List[Instruction]): (Horizontal, Depth) =
    instructions
      .foldLeft((0, 0)) { (seed, current) =>
        current.direction match {
          case "forward" => (seed._1 + current.amount, seed._2)
          case "up"      => (seed._1, seed._2 - current.amount)
          case _         => (seed._1, seed._2 + current.amount)
        }
      }

  private def calculatePositionWithAim(instructions: List[Instruction]): (Horizontal, Depth, Aim) =
    instructions
      .foldLeft((0, 0, 0)) { (seed, current) =>
        current.direction match {
          case "forward" => (seed._1 + current.amount, seed._2 + (seed._3 * current.amount), seed._3)
          case "up"      => (seed._1, seed._2, seed._3 - current.amount)
          case _         => (seed._1, seed._2, seed._3 + current.amount)
        }
      }

  private def processInput(path: String) =
    Files[IO]
      .readUtf8Lines(Path(path))
      .through(convertInputToInstruction)
      .compile
      .toList

  override def run: IO[Unit] =
    for {
      res <- processInput(part2Path)
               .map(calculatePosition)
               .map(r => r._1 * r._2)
      _ <- IO(println(res))
      res2 <- processInput(part2Path)
                .map(calculatePositionWithAim)
                .map(r => r._1 * r._2)
      _ <- IO(println(res2))
    } yield ()
}
