package chapter22.errormanagement

import zio.clock.Clock
import zio.duration.durationInt
import zio.test.Assertion._
import zio.test.TestAspect.timeout
import zio.test._
import zio.{CanFail, Chunk, IO, Ref, Schedule, UIO, ZIO, clock}

import scala.language.postfixOps

object ErrorManagementSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ScheduleSpec")(
      suite("Repeat on success according to a provided strategy")(
        testM("recurs(1)") {
          for {
            ref <- Ref.make(0)
            _ <- ref.update(_ + 2).repeat(Schedule.recurs(3))
            res <- ref.get
          } yield assert(res)(equalTo(8))
        },
        testM("for 'once' does repeats 1 additional time") {
          for {
            ref <- Ref.make(0)
            _   <- ref.update(_ + 2).repeat(Schedule.once)
            res <- ref.get
          } yield assert(res)(equalTo(4))
        },
        testM("retry 0 time for `once` when first time succeeds") {
          implicit val canFail = CanFail
          for {
            ref: Ref[Int] <- Ref.make(0)
            x: IO[Nothing, Unit] = ref.update(_ + 1)
            _: Unit <- ref.update(_ + 1).retry(Schedule.once)
            i   <- ref.get
          } yield assert(i)(equalTo(1))
        },
        testM("retry exactly one time for `once` when second time succeeds") {
          // one retry on failure
          for {
            ref <- Ref.make(0)
            _   <- failOn0(ref).retry(Schedule.once)
            r   <- ref.get
          } yield assert(r)(equalTo(2))
        },
        testM("for 'recurWhile(cond)' repeats while the cond still holds") {
          def cond: Int => Boolean = _ < 10
          assertM(repeatSchedule(Schedule.recurWhile(cond)))(equalTo(10))
        },
        testM("for 'recurWhileM(cond)' repeats while the effectful cond still holds") {
          def cond: Int => UIO[Boolean] = x => IO.succeed(x == 4777777)
          assertM(repeatSchedule(Schedule.recurWhileM(cond)))(equalTo(1))
        }
      ),
      suite("Simulate a schedule")(
        testM("without timing out") {
          val schedule  = Schedule.exponential(1.minute)
          val scheduled = clock.currentDateTime.orDie.flatMap(schedule.run(_, List.fill(5)(())))
          val expected  = Chunk(1.minute, 2.minute, 4.minute, 8.minute, 16.minute)
          assertM(scheduled)(equalTo(expected))
        } @@ timeout(1.seconds)
      )
    )

  def repeatSchedule[B](
      schedule: Schedule[Any, Int, B]
  ): ZIO[Any with Clock, Nothing, B] =
    for {
      ref <- Ref.make(0)
      res <- ref.updateAndGet(_ + 1).repeat(schedule)
    } yield res

  /**
   * A function that increments ref each time it is called.
   * It returns either a failure if ref value is 0 or less
   * before increment, and the value in other cases.
   */
  def failOn0(ref: Ref[Int]): IO[String, Int] =
    for {
      i <- ref.updateAndGet(_ + 1)
      x <- if (i <= 1) IO.fail(s"Error: $i") else IO.succeed(i)
    } yield x
}
