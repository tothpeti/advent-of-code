package year2022

import cats.effect.{IO, IOApp}
import fs2.Pipe
import fs2.io.file.{Files, Path}

object Day2 extends IOApp.Simple {

  // private val testPath = "src/main/resources/year2022/day2/test.txt"
  // private val part1Path = "src/main/resources/year2022/day2/part1.txt"
  private val part2Path = "src/main/resources/year2022/day2/part2.txt"

  private final case class Match(opponent: String, guess: String)

  private sealed trait Outcome
  private final object Win  extends Outcome
  private final object Draw extends Outcome
  private final object Loss extends Outcome

  /*
  Rock (A or X)
  Paper (B or Y)
  Scissor (C or Z)
   */

  // used for getting the point that is assigned to a given guess
  private val guessMap: Map[String, Int] = Map(
    "X" -> 1,
    "Y" -> 2,
    "Z" -> 3
  )

  // Converts outcome to the amount of point that we will get
  private val pointsMap: Map[Outcome, Int] = Map(
    Win  -> 6,
    Draw -> 3,
    Loss -> 0
  )

  // Used for evaluating the match outcome
  private val selectedMap: Map[String, Outcome] = Map(
    "AY" -> Win,
    "BZ" -> Win,
    "CX" -> Win,
    "AX" -> Draw,
    "BY" -> Draw,
    "CZ" -> Draw,
    "AZ" -> Loss,
    "BX" -> Loss,
    "CY" -> Loss
  )

  // For Part 2 solution
  // In Part 2 the guess will tell what should be the outcome
  private val modifiedOutcomeMap: Map[String, Outcome] = Map(
    "X" -> Loss,
    "Y" -> Draw,
    "Z" -> Win
  )

  private val convertInputToMatch: Pipe[IO, String, Match] = stream =>
    stream
      .map(_.split(" "))
      .map {
        case Array(opponent, guess) =>
          Match(opponent, guess)
        case _ =>
          throw new IllegalStateException("Something went wrong! This should not happen...")
      }

  private def processInput(path: String): IO[List[Match]] =
    Files[IO]
      .readUtf8Lines(Path(path))
      .through(convertInputToMatch)
      .compile
      .toList

  private def calculateOutcome(result: String, map: Map[String, Outcome]): Outcome =
    map.get(result) match {
      case Some(Win)  => Win
      case Some(Draw) => Draw
      case Some(Loss) => Loss
      case None =>
        throw new IllegalStateException("Something went wrong! This should not happen...")
    }

  private def calculateTotalScore(matches: List[Match], f: (String, Map[String, Outcome]) => Outcome): IO[Int] =
    IO {
      matches
        .foldLeft(0) { (seed, current) =>
          // Part 1
          //val outcomeScore  = pointsMap(f(current.opponent + current.guess, selectedMap))
          //val selectedScore = guessMap(current.guess)
          //seed + (outcomeScore + selectedScore)

          // Part 2
          val outcome = f(current.guess, modifiedOutcomeMap)
          val selectedTuple =
            selectedMap.toList.find { selected =>
              selected._1.contains(current.opponent) && selected._2 == outcome
            }.getOrElse(throw new IllegalStateException("Something went wrong! This should not happen..."))

          val selectedScore =
            selectedTuple._1.split("") match {
              case Array(_, second) =>
                guessMap(second)

              case _ =>
                throw new IllegalStateException("Something went wrong! This should not happen...")
            }

          seed + (pointsMap(outcome) + selectedScore)
        }
    }

  override def run: IO[Unit] =
    for {
      matches <- processInput(part2Path)
      res     <- calculateTotalScore(matches, calculateOutcome)
      _       <- IO(println(res))
    } yield ()

}
