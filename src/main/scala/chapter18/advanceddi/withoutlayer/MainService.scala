package chapter18.advanceddi.withoutlayer

import zio.{UIO, URIO, ZIO}

trait MainService {

  def getUserAndPrint(userId: Int): ZIO[RandomService, Nothing, User]
}

case class User(id: Int, name: String)

trait RandomService {
  def getRandomString: UIO[String]
}

trait Database {
  def getUser(userId: Int): URIO[RandomService, User]
}

trait Logging {
  def logLine(line: String): UIO[Unit]
}

class MainServiceImpl(db: Database, logging: Logging) extends MainService {

  override def getUserAndPrint(userId: Int): ZIO[RandomService, Nothing, User] =
    for {
      user <- db.getUser(userId)
      _ <- logging.logLine(s"Got $user")
    } yield user

  def dbEffect(userId: Int): ZIO[RandomService with Database, Nothing, User] = ZIO.environment[Database].flatMap(_.getUser(userId))

  def loggingEffect(line: String): ZIO[Logging, Nothing, Unit] = ZIO.environment[Logging].flatMap(_.logLine(line))

  def getUserAndPrint2(userId: Int): ZIO[RandomService, Nothing, User] = {
    val effect: ZIO[Logging with RandomService with Database, Nothing, User] = for {
      user <- dbEffect(userId)
      _ <- loggingEffect(s"Got $user")
    } yield user

    effect.provideSome[RandomService](env => new Logging with Database with RandomService {
      override def logLine(line: String): UIO[Unit] = logging.logLine(line)

      override def getUser(userId: Int): URIO[RandomService, User] = db.getUser(userId)

      override def getRandomString: UIO[String] = env.getRandomString
    })
  }

}