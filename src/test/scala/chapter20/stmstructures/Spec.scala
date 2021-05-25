package chapter20.stmstructures

import zio.clock._
import zio.console.{ putStrLn, _ }
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect.{ ignore, timeout }
import zio.test._
import zio.test.environment.{ TestClock, TestConsole }
import zio._
import zio.stm._
import java.util.concurrent.atomic.AtomicInteger

import scala.language.postfixOps

object Spec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("STM Data structures")(
      testM("TArray swapping elements") {
        for {
          a     <- TArray.make(1, 2, 3, 4, 5).commit
          _     <- Helper.ararySwap(a, 0, 1).commit
          aList <- a.toList.commit
        } yield assert(aList)(equalTo(List(2, 1, 3, 4, 5)))
      },
      testM("TArray transform instead of map") {
        val list = (for {
          array <- TArray.make(1, 2, 3)
          _     <- array.transform(_ + 1)
          list  <- array.toList
        } yield list).commit

        assertM(list)(equalTo(List(2, 3, 4)))
      },
      test("TArray concurrent transforms") {
        val atomicCounter = new AtomicInteger(0)
        val listEffect    = for {
          array <- TArray.make(1, 2, 3).commit
          _     <- ZIO.foreachPar_(1 to 1000) { _ =>
                     array
                       .transform {
                         atomicCounter.getAndIncrement()
                         _ + 1
                       }
                       .commit
                       .fork
                   }
          list  <- array.toList.commit
        } yield list

        val list = Runtime.default.unsafeRun(listEffect)
        println(s"TArray concurrent transforms atomicCounter is $atomicCounter")
        assert(list)(
          equalTo(List(1001, 1002, 1003))
        )
      },
      //both calls will be made but only one value is stored in the end
      testM("TMap get or else update") {
        for {
          map   <- TMap.make[String, String]().commit
          v1    <- Helper
                     .mapGetOrElseUpdate(map)(
                       "key", {
                         println("Calculating... 1")
                         "val1"
                       }
                     )
                     .commit
                     .fork
          v2    <- Helper
                     .mapGetOrElseUpdate(map)(
                       "key", {
                         println("Calculating... 2")
                         "val2"
                       }
                     )
                     .commit
                     .fork
          v1Val <- v1.join
          v2Val <- v2.join
          _      = println(v1Val)
          _      = println(v2Val)
        } yield assert(v1Val)(equalTo(v2Val)) && assert(v2Val)(equalTo("val1") || equalTo("val2"))
      },
      testM("TPromise") {
        for {
          tPromise <- TPromise.make[String, Int].commit
          fiber1   <- tPromise.await.commit.fork
          fiber2   <- tPromise.await.commit.fork
          _        <- tPromise.succeed(0).commit.fork
          f1Val    <- fiber1.join.either
          f2Val    <- fiber1.join.either
        } yield assert(f1Val.getOrElse(1))(equalTo(0)) && assert(f2Val.getOrElse(1))(equalTo(0))
      },
      testM("TQueue") {
        for {
          tQueue <- TQueue.bounded[Int](5).commit
          fiber1 <- tQueue.take.commit.fork
          fiber2 <- tQueue.take.commit.fork
          _      <- tQueue.offerAll(List(1, 2)).commit
          f1Val  <- fiber1.join
          f2Val  <- fiber2.join
        } yield assert(f1Val)(not(equalTo(f2Val)))
      },
      testM("TReentrantLock") {
        for {
          lock           <- TReentrantLock.make.commit
          numberOfLocks1 <- lock.acquireRead.commit
          numberOfLocks2 <- lock.acquireRead.commit
        } yield assert(numberOfLocks1)(equalTo(1)) && assert(numberOfLocks2)(equalTo(2))
      },
      testM("TSemaphore") {
        val stm = for {
          _ <- TRef.make(0)
          _  = println("Got a permit")
          _  = println("Leaving...")
        } yield ()

        for {
          sem    <- TSemaphore.make(1L).commit
          fibers <- ZIO.collectAll(ZIO.replicate(10)(sem.withPermit(stm).commit.fork))
          _      <- ZIO.foreach(fibers)(_.join)
        } yield assert(())(isUnit)
      }
    )
}

object Helper {

  def ararySwap[A](array: TArray[A], i: Int, j: Int): STM[Nothing, Unit] =
    for {
      a1 <- array(i)
      a2 <- array(j)
      _  <- array.update(i, _ => a2)
      _  <- array.update(j, _ => a1)
    } yield ()

  def mapGetOrElseUpdate[K, V](map: TMap[K, V])(k: K, v: => V): STM[Nothing, V] =
    map.get(k).flatMap {
      case Some(v) => STM.succeed(v)
      case None    => STM.succeed(v).flatMap(v => map.put(k, v).as(v))
    }
}
