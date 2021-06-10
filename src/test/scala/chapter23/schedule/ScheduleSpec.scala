package chapter23.schedule

import zio._
import zio.console.putStrLn
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, ZSpec, _}

object ScheduleSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Schedule")(
      testM("Schedule map") {
        putStrLn("").repeat(Schedule.recurs(3).map(_.toString).fold("")(_ + _)).map(ls => assert(ls)(equalTo("012")))
      },
      testM("Schedule contramap") {
        val schedule: Schedule[Any, String, Long] = Schedule.recurs(3)
        val contramapped: Schedule[Any, Int, Long] = schedule.contramap((a: Int) => a.toString)
        ZIO.succeed(1).repeat(contramapped).map(ls => assert(ls)(equalTo(3L)))
      },
      testM("Schedule fold") {
        putStrLn("").repeat(Schedule.recurs(4).fold(0L)(_ + _)).map(ls => assert(ls)(equalTo(6L)))
      },
      testM("Schedule collect") {
        putStrLn("").repeat(Schedule.recurs(3).collectAll).map(ls => assert(ls)(equalTo(Chunk(0L, 1L, 2L))))
      }
    )

}
