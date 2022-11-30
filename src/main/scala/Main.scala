import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp.Simple {
  override def run: IO[Unit] =
    IO(println("Hello World!")).as(ExitCode.Success)
}
