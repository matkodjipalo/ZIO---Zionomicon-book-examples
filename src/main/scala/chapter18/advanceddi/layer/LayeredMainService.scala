package chapter18.advanceddi.layer

import chapter18.advanceddi.layer.random.Random
import zio.{Has, UIO, URIO, ZIO, ZLayer}

trait MainService {

  def getUserAndPrint(userId: Int): ZIO[Random, Nothing, mixed.User]
}

case class User(id: Int, name: String)

object random {
  type Random = Has[Random.Service]

  object Random {

    trait Service {
      def getRandomString: UIO[String]
    }

    val randomService = ZLayer.succeed {
      new Service {
        override def getRandomString: UIO[String] = UIO.succeed("abc")
      }
    }
  }

  def getRandomString: URIO[Random, String] =
    ZIO.accessM(_.get.getRandomString)
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
      def getUser(userId: Int): UIO[mixed.User]
    }

    val live: ZLayer[Random, Nothing, Database] = ZLayer.fromService { randomService =>
      new Service {
        def getUser(userId: Int): UIO[mixed.User] =
          randomService.getRandomString.flatMap { name =>
            UIO.succeed(mixed.User(userId, name))
          }
      }
    }

    def getUser(userId: Int): URIO[Database, mixed.User] = ZIO.accessM(_.get.getUser(userId))
  }

}

class MainServiceImpl() extends MainService {

  import chapter18.advanceddi.layer.database._
  import chapter18.advanceddi.layer.logging._

  override def getUserAndPrint(userId: Int): ZIO[Any, Nothing, mixed.User] = {

    val z: ZIO[Logging with Database, Nothing, mixed.User] = for {
      user <- Database.getUser(userId)
      _ <- Logging.logLine(s"Got $user")
    } yield user

    z.provideLayer(Logging.console ++ (Random.randomService >>> Database.live))
  }


}