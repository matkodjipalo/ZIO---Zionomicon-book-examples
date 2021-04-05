name := "ZIO"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "dev.zio" %% "zio" % "1.0.5"
libraryDependencies +=
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-test" % "1.0.5",
  "dev.zio" %% "zio-test-sbt" % "1.0.5"
)
