ThisBuild / scalaVersion := "2.13.10"

version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"   % "2.9.0",
  "org.typelevel" %% "cats-effect" % "3.4.1",
  "co.fs2"        %% "fs2-core"    % "3.4.0",
  "co.fs2"        %% "fs2-io"      % "3.4.0",
  compilerPlugin("com.github.ghik" %% "zerowaste" % "0.2.1" cross CrossVersion.full)
)

scalacOptions ++= Seq(
  "-Xfatal-warnings"
)

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
