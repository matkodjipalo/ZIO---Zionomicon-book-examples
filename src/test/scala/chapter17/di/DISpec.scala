package chapter17.di

import zio.test.Assertion._
import zio.test._
import zio.{ Ref, Task, UIO, ZIO }

import scala.language.postfixOps

object DISpec extends DefaultRunnableSpec {

  type UserID = String
  case class UserProfile(name: String)

  // The database module:
  trait Database {
    val database: Database.Service
  }

  object Database {
    // The database module contains the database service:
    trait Service {
      def lookup(id: UserID): Task[UserProfile]
      def update(id: UserID, profile: UserProfile): Task[Unit]
    }
  }

  // The logger module:
  trait Logger {
    def logger: Logger.Service
  }

  object Logger {
    // The logger module contains the logger service:
    trait Service {
      def info(id: String): Task[Unit]
    }
  }

  // A concurrent-safe test database service, which uses a `Ref` to keep track
  // of changes to the test database state:
  class DatabaseTestService(ref: Ref[DatabaseTestService.State])
    extends Database.Service {
    def lookup(id: UserID): Task[UserProfile] =
      ref.modify(_.lookup(id)).flatMap(option => Task(option.get))

    def update(id: UserID, profile: UserProfile): Task[Unit] =
      ref.update(_.update(id, profile)).unit
  }

  object DatabaseTestService {
    // The database state, which keeps track of the data as well as a log of
    // database operations performed against the database:
    final case class State(map: Map[UserID, UserProfile], ops: List[String]) {
      def lookup(id: UserID): (Option[UserProfile], State) =
        (map.get(id), log(s"Lookup(${id})"))

      def update(id: UserID, profile: UserProfile): State =
        copy(map = map + (id -> profile)).log(s"Update(${id}, ${profile})")

      def log(op: String): State = copy(ops = op :: ops)
    }
  }

  // A concurrent-safe test logger service, which uses a `Ref` to keep track
  // of log output:
  class LoggerTestService(ref: Ref[Vector[String]]) extends Logger.Service {
    def info(line: String): Task[Unit] = ref.update(_ :+ line).unit
  }

  val lookedUpProfile: ZIO[Database with Logger, Throwable, UserProfile] =
    ZIO.accessM[Logger with Database] { modules =>
      import modules.{database, logger}

      for {
        profile <- database.lookup("abc")
        _ <- logger.info(profile.name)
      } yield profile
    }

  def testScenario[E, A](state: DatabaseTestService.State)(
    eff: ZIO[Database with Logger, E, A]
  ): UIO[(Either[E, A], DatabaseTestService.State, Vector[String])] =
    for {
      databaseRef <- Ref.make(state)
      loggerRef <- Ref.make(Vector.empty[String])
      // Construct a new environment for the effect being tested:
      env: Database with Logger = new Database with Logger {
        val database = new DatabaseTestService(databaseRef)
        val logger = new LoggerTestService(loggerRef)
      }
      either <- eff.provide(env).either
      dbState <- databaseRef.get
      loggerState <- loggerRef.get
    } yield (either, dbState, loggerState)

  def spec: ZSpec[Environment, Failure] =
    suite("DISpec")(
      testM("Searching a db for user profile with testName") {
        for {
          v <- testScenario(
            DatabaseTestService
              .State(Map("abc" -> UserProfile("testName")), Nil)
          )(lookedUpProfile)
        } yield {
          assert(v._3)(equalTo(Vector("testName")))
        }
      }
    )
}
