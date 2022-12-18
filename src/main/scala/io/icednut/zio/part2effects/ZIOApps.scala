package io.icednut.zio.part2effects

import zio.*

object ZIOApps {

  val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    given trace: Trace = Trace.empty

    Unsafe.unsafeCompat { unsafe =>
      given u: Unsafe = unsafe

      println(runtime.unsafe.run(meaningOfLife))
    }
  }
}

object BetterApp extends ZIOAppDefault {

  // provides runtime, trace, ...
  override def run = ZIOApps.meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
}
