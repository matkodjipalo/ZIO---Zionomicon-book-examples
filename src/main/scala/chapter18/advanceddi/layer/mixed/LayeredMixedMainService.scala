package chapter18.advanceddi.layer.mixed
import zio.{Has, UIO, URIO, ZIO, ZLayer}

trait LayeredMixedMainService {

  def getUserAndPrint(userId: Int): ZIO[RandomService, Nothing, User]
}

case class User(id: Int, name: String)

trait RandomService {
  def getRandomString: UIO[String]
}

class RandomServiceImpl extends RandomService {
  override def getRandomString: UIO[String] = UIO.succeed("abc")
}

object logging {
  type Logging = Has[Logging.Service]

  object Logging {

    trait Service {
      def logLine(line: String): UIO[Unit]

    }

    val console: ZLayer[Any, Nothing, Logging] = ZLayer.succeed {
      new Service {
        def logLine(line: String): UIO[Unit] =
          UIO.effectTotal(println(line))
      }
    }

    def logLine(line: String): URIO[Logging, Unit] = ZIO.accessM(_.get.logLine(line))
  }

}

object database {
  type Database = Has[Database.Service]

  object Database {

    trait Service {
      def getUser(userId: Int): UIO[User]
    }

    val live: ZLayer[Has[RandomService], Nothing, Database] = ZLayer.fromService { randomService =>
      new Service {
        override def getUser(userId: Int): UIO[User] =
          randomService.getRandomString.map(name => User(userId, name))
      }
    }

    def getUser(userId: Int): URIO[Database, User] = ZIO.accessM(_.get.getUser(userId))
  }

}

// RandomService could be Injected by Guice for example
class LayeredMixedMainServiceImpl (randomService: RandomService) extends LayeredMixedMainService {

  import database._
  import logging._

  override def getUserAndPrint(userId: Int): UIO[User] = {

    val program: ZIO[Logging with Database, Nothing, User] = for {
      user <- Database.getUser(userId)
      _ <- Logging.logLine(s"Got $user")
    } yield user

    program.provideLayer(ZLayer.succeed(randomService) >>> (Database.live ++ Logging.console))
  }

}