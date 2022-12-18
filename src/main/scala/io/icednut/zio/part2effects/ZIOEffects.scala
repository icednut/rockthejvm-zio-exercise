package io.icednut.zio.part2effects

import zio.*

import scala.io.StdIn

object ZIOEffects:
  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }

  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   * Type aliases of ZIOs
   */
  // UIO[A] = ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)
  // URIO[R, A] = ZIO[R, Nothing, A] - cannot fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)
  // RIO[R, A] = ZIO[R, Throwable, A] - can fail with a Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))
  // Task[A] = ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, producesA
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))
  // IO[E, A] = ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")

  /**
   * Exercises
   */
  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = for {
    _ <- zioa
    b <- ziob
  } yield {
    b
  }

  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob

  // 2 - sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = for {
    a <- zioa
    _ <- ziob
  } yield {
    a
  }

  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa <* ziob

  // 3 - run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap(_ => runForever(zio))

  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever(zio)

  def endlessLoop = runForever {
    ZIO.succeed {
      println("running....")
      Thread.sleep(1000L)
    }
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = zio.map { _ => value }

  def convert_v2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = zio.as(value)

  // 5 - discard the value of a ZIO to Unit/
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = zio.map { _ => () }
  def asUnit_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = convert(zio, ())
  def asUnit_v3[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = zio.unit

  // 6 - recursion
  def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)
  def sumZIO(n: Int): UIO[Int] =
    if (n == 0) {
      ZIO.succeed(0)
    } else {
      sumZIO(n - 1).map(_ + n)
    }

  // 7 - fibonacci
  // hint: use ZIO.suspend/ZIO.suspendSucceed
  def fiboZIO(n: Int): UIO[BigInt] =
    if (n <= 2) {
      ZIO.succeed(1)
    } else {
      for {
        a <- ZIO.suspendSucceed(fiboZIO(n - 1))
        b <- fiboZIO(n - 2)
      } yield {
        a + b
      }
    }

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty

//    // 1
//    Unsafe.unsafe { implicit u =>
//      val firstEffect = ZIO.succeed {
//        println("computing first effect")
//        Thread.sleep(1000L)
//        1
//      }
//      val secondEffect = ZIO.succeed {
//        println("computing second effect")
//        Thread.sleep(1000L)
//        2
//      }
//
//      val result = sequenceTakeLast(firstEffect, secondEffect)
//      println(runtime.unsafe.run(result))
//    }

//    // 2
//    Unsafe.unsafe { implicit u =>
//      val firstEffect = ZIO.succeed {
//        println("computing first effect")
//        Thread.sleep(1000L)
//        1
//      }
//      val secondEffect = ZIO.succeed {
//        println("computing second effect")
//        Thread.sleep(1000L)
//        2
//      }
//
//      val result = sequenceTakeFirst(firstEffect, secondEffect)
//      println(runtime.unsafe.run(result))
//    }

//    // 3
//    Unsafe.unsafe { implicit u =>
//      val effect = ZIO.succeed {
//        println("running")
//        Thread.sleep(1000L)
//        1
//      }
//
//      val result = runForever(effect)
//      println(runtime.unsafe.run(result))
//    }

    // fibo
    Unsafe.unsafe { implicit u =>
      val result = fiboZIO(20)
      println(runtime.unsafe.run(result))
    }
  }