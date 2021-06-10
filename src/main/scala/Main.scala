import zio.console.putStrLn
import zio.duration._
import zio.{ExitCode, Schedule, URIO}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Main extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    putStrLn(LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME))
//      .repeat(Schedule.spaced(1.second).addDelay(d => (d + 1).second) && Schedule.recurs(3))
            .repeat(Schedule.spaced(1.second).addDelay(d => (d + 1).second) || Schedule.recurs(3))
      .exitCode
}
