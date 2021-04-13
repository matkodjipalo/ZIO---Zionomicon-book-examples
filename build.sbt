name := "ZIO"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "dev.zio" %% "zio" % "1.0.5"
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-test"          % "1.0.5" % "test",
  "dev.zio" %% "zio-test-sbt"      % "1.0.5" % "test",
  "dev.zio" %% "zio-test-magnolia" % "1.0.5" % "test" // optional
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
