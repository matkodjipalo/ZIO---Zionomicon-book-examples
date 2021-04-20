package chapter15.composableresources

import zio.clock._
import zio.console.{ putStrLn, _ }
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect.{ ignore, timeout }
import zio.test._
import zio.test.environment.{ TestClock, TestConsole }
import zio.{ Fiber, Queue, URIO, ZIO, ZQueue }

import scala.language.postfixOps
import java.io.IOException
import zio.Task
import zio.ZManaged
import zio.Reservation
import zio.Exit.Failure
import zio.Exit.Success
import zio.UIO

object ComposableResourcesSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Weather examples")(
      testM("Composing brachets - simple solution") {
        lazy val analyzeWeatherData: Task[(String, String)] =
          Helpers.withFile("temperatures.txt") { weatherData =>
            Helpers.withFile("results.txt") { results =>
              Helpers.analyze(weatherData, results, "Weather examples")
            }
          }
        assertM(analyzeWeatherData)(equalTo("temperatures.txt", "results.txt"))
      },
      testM("Using ZManaged.zipPar") {
        lazy val weatherDataAndResults =
          Helpers.file("temperatures.txt").zipPar(Helpers.file("results.txt"))
        lazy val analyzeWeatherData    =
          weatherDataAndResults.use {
            case (weatherData, results) =>
              Helpers.analyze(weatherData, results, "Using ZManaged.zipPar")
          }
        assertM(analyzeWeatherData)(equalTo("temperatures.txt", "results.txt"))
      },
      testM("Using ZManaged.zipPar and fileMakeExit") {
        lazy val weatherDataAndResults =
          Helpers.fileMakeExit("temperatures.txt").zipPar(Helpers.fileMakeExit("results.txt"))
        lazy val analyzeWeatherData    =
          weatherDataAndResults.use {
            case (weatherData, results) =>
              Helpers.analyze(weatherData, results, "Using ZManaged.zipPar and fileMakeExit")
          }
        assertM(analyzeWeatherData)(equalTo("temperatures.txt", "results.txt"))
      },
      testM("Using list of ZManaged with foreach") {
        val fileNames        =
          List("temperatures_2018.txt", "temperatures_2019.txt", "temperatures_2020.txt", "temperatures_2021.txt")
        val files            =
          ZManaged.foreach(fileNames)(Helpers.file)
        lazy val weatherData =
          files.use { files =>
            ZIO.foreach(files) { file =>
              ZIO.succeed(s"Doing something with $file")
            }
          }

        assertM(weatherData)(equalTo(fileNames.map(filename => s"Doing something with $filename")))
      },
      testM("Using list of ZManaged with foreachPar") {
        val fileNames        =
          List("temperatures_2018.txt", "temperatures_2019.txt", "temperatures_2020.txt", "temperatures_2021.txt")
        val files            =
          ZManaged.foreachPar(fileNames)(Helpers.file)
        lazy val weatherData =
          files.use { files =>
            ZIO.foreach(files) { file =>
              ZIO.succeed(s"Doing something with $file")
            }
          }

        assertM(weatherData)(equalTo(fileNames.map(filename => s"Doing something with $filename")))
      },
      testM("Converting zio effects to ZManaged and back to ZIO") {
        lazy val effectWithFinalizer    =
          Helpers.zioEffectToManagedWithFinalizer(Task.succeed("Task1 succeeded!"), "Called from finalizer")
        lazy val effectWithOutFinalizer =
          Helpers.zioEffectToManagedWithoutFinalizer(Task.succeed("Task2 succeeded!"))
        for {
          first  <- effectWithFinalizer.use(res => putStr(res))
          second <- effectWithOutFinalizer.use(res => putStr(res))
          output <- TestConsole.output
        } yield assert(output)(
          equalTo(
            Vector(
              "Task1 succeeded!",
              "Greeting from finalizer with previous message: Task1 succeeded!. Still all good, Called from finalizer",
              "Task2 succeeded!"
            )
          )
        )
      }
    )
}

object Helpers {

  def openFile(name: String): UIO[String] =
    ZIO.effectTotal {
      println(s"Opened file with name $name")
      name
    }

  def closeFile(name: String): UIO[String] =
    ZIO.effectTotal {
      println(s"Closed file with name $name")
      name
    }

  def withFile[A](name: String)(
      use: String => Task[A]
  ): Task[A] =
    openFile(name).bracket(closeFile)(use)

  def analyze(weatherData: String, results: String, testName: String): Task[(String, String)] =
    ZIO.effectTotal {
      println(s"Analyzing files $weatherData and $results in test $testName")
      (weatherData, results)
    }

  def filePrimitive(name: String): ZManagedPrimitive[Any, Throwable, String] =
    ZManagedPrimitive(openFile(name), closeFile)

  def file(name: String): ZManaged[Any, Throwable, String] =
    ZManaged.make(openFile(name))(closeFile)

  def fileReserved(name: String): ZManaged[Any, Throwable, String] =
    ZManaged.makeReserve(
      ZIO.succeed(
        Reservation(
          openFile(name),
          res =>
            res match {
              case Failure(cause) => ZIO.succeed("Acquire failed")
              case Success(value) => ZIO.succeed(s"Closing file with name: $value")
            }
        )
      )
    )

  //not sure about this one, different signature than in book?
  def fileMakeExit(name: String): ZManaged[Any, Throwable, String]                       =
    ZManaged.makeExit(openFile(name)) { case (resource, _) => closeFile(resource) }

  def zioEffectToManagedWithFinalizer(basicTask: Task[String], finalizerMessage: String) =
    basicTask.toManaged(prevMsg =>
      putStr(s"Greeting from finalizer with previous message: $prevMsg. Still all good, $finalizerMessage")
    )

  def zioEffectToManagedWithoutFinalizer(basicTask: Task[String]) =
    basicTask.toManaged_

}
