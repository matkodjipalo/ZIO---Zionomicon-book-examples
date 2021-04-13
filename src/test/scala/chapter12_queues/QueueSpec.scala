package chapter12_queues

import zio.clock._
import zio.console.{putStrLn, _}
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect.{ignore, timeout}
import zio.test._
import zio.test.environment.{TestClock, TestConsole}
import zio.{Fiber, Queue, URIO, ZIO, ZQueue}

import scala.language.postfixOps

object QueueSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ZQueueSpec")(
      testM("sequential offer and take - FIFO") {
        for {
          queue <- Queue.unbounded[Int]
          o1 <- queue.offer(1)
          o2 <- queue.offer(2)
          t1 <- queue.take
          t2 <- queue.take
        } yield assert(t1)(equalTo(1)) &&
          assert(t2)(equalTo(2)) &&
          assert(o1)(isTrue) &&
          assert(o2)(isTrue)
      },
      testM("take semantically blocks while waiting for a new offer") {
        for {
          queue <- Queue.unbounded[Int]
          f <- queue.take.flatMap(n => putStrLn(s"Got $n!")).forever.fork
          _ <- queue.offer(1)
          _ <- queue.offer(2)
          suspended <-
            f.status
              .collect(()) {
                case Fiber.Status.Suspended(_, _, _, suspended, _) =>
                  suspended
              }
              .eventually
        } yield assert(suspended)(equalTo(List(f.id)))
      },
      testM("there is no risk that two workers will take the same value") {
        def work(identifier: String)(n: Int): URIO[Clock with Console, Unit] =
          putStrLn(
            s"fiber $identifier starting some expensive work with input $n"
          ) *>
            ZIO.sleep(1.second) *>
            putStrLn(s"fiber $identifier finished with input $n")

        for {
          queue <- Queue.unbounded[Int]
          _ <- queue.take.flatMap(work("left")).forever.fork
          _ <- queue.take.flatMap(work("right")).forever.fork
          _ <- ZIO.foreach_(1 to 10)(queue.offer)
          _ <- TestClock.adjust(1.minute)
          output <- TestConsole.output
          numsFromOutput =
            output.map(_.split("\\D+").filter(_.nonEmpty).toList.head.toInt)
        } yield assert(numsFromOutput.length)(equalTo(20)) && assert(
          numsFromOutput.distinct.sorted.toList
        )(equalTo(List.range(1, 11)))
      },
      testM("back pressure strategy") {
        for {
          queue <- Queue.bounded[String](2)
          _ <- queue.offer("ping").tap(_ => putStrLn("ping")).forever.fork
          _ <- TestClock.adjust(100 days)
          output <- TestConsole.output
          _ <- queue.take
        } yield assert(output)(equalTo(Vector("ping\n", "ping\n")))
      },
      testM("sliding strategy") {
        for {
          queue <- Queue.sliding[Int](2)
          _ <- ZIO.foreach_(List(1, 2, 3))(queue.offer)
          a <- queue.take
          b <- queue.take
        } yield assert(a, b)(equalTo(2, 3))
      },
      testM("dropping strategy") {
        for {
          threeDayTraining <- Queue.dropping[String](3)
          _ <- ZIO.foreach_(List("pecs", "shoulders", "arms"))(
            threeDayTraining.offer
          )
          legs <- threeDayTraining.offer("legs")
          training <- threeDayTraining.takeAll
        } yield assert(training)(
          equalTo(List("pecs", "shoulders", "arms"))
        ) && assert(legs)(isFalse)
      },
      testM("transforming output") {
        for {
          queue <- Queue.bounded[Int](3)
          mapped = queue.map(_.toString)
          _ <- mapped.offer(1)
          s <- mapped.take
        } yield assert(s)(equalTo("1"))
      },
      testM("transforming input") {
        for {
          queue <- Queue.unbounded[Int].map(_.contramap[List[String]](_.length))
          _ <- queue.offer(List("Hello", "ZQueue"))
          n <- queue.take
        } yield assert(n)(equalTo(2))
      },
      testM("filtering input") {
        for {
          queue <- Queue.unbounded[Int].map(_.filterInput[Int](_ % 2 == 0))
          _ <- queue.offerAll(List(1, 2, 3, 4, 5))
          evens <- queue.takeAll
        } yield assert(evens)(equalTo(List(2, 4)))
      },
      testM("filtering output") {
        for {
          queue <-
            Queue
              .unbounded[String]
              .map(_.filterOutput(_.toUpperCase.contains("ANA")))
          _ <- queue.offerAll(List("Ana", "voli", "Milovana"))
          res <- queue.takeAll
        } yield assert(res)(equalTo(List("Ana", "Milovana")))
      },
      ignore(testM("combining queues") {
        for {
          left <- Queue.unbounded[Helper.Name]
          right <- Queue.unbounded[Helper.Name]
          both: ZQueue[Any, Any, Nothing, Nothing, Helper.Name, (Helper.Name, Helper.Name)] = left && right
          _ <- Helper.leftWorker(left)
          _ <- Helper.rightWorker(right)
          _ <- both.offer(Helper.Name("Jane", "Doe"))
          l <- left.take
          r <- right.take
        } yield assert(l, r)(
          equalTo(Helper.Name("Jane", "Doe"), Helper.Name("Jane", "Doe"))
        )
      }) @@ timeout(5000.millis)
    )
}

object Helper {
  def leftWorker(queue: Queue[Name]) =
    queue.take
      .flatMap { name =>
        putStrLn(s"Got first name ${name.first}")
      }
      .forever
      .fork
      .unit

  def rightWorker(queue: Queue[Name]) =
    queue.take
      .flatMap { name =>
        putStrLn(s"Got last name ${name.last}")
      }
      .forever
      .fork
      .unit

  final case class Name(first: String, last: String)
}
