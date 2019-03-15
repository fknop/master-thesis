package village1.benchmark

import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.VillageOneModel

object PrecomputeBenchmark extends App {

  val T = Array(5, 10, 15)
  val D = Array(5, 10, 20, 30, 40, 50)
  val W = Array(50, 100, 150, 200, 250, 300)

  val measurements = Array.fill(T.length, D.length, W.length)(0L)
  val repeat = 10

  for (r <- 0 to repeat) {
    for (i <- T.indices) {
      val t = T(i)
      for (j <- D.indices) {
        val d = D(j)
        for (k <- W.indices) {
          val w = W(k)
          val problem = InstanceGenerator.generate(
            InstanceOptions(
              t = t,
              clients = d,
              demands = d,
              workers = w,
              skills = 10
            )
          )

          val model = new VillageOneModel(problem)
          val time = model.precomputeTime

          // Make sure the JVM has JIT compiled the code before measuring
          if (r > 0) {
            measurements(i)(j)(k) += time
          }
        }
      }
    }
  }


  val avg = Array.fill(T.length, D.length, W.length)(0.0)

  for (i <- measurements.indices) {
    for (j <- measurements(i).indices) {
      for (k <- measurements(i)(j).indices) {
        avg(i)(j)(k) = measurements(i)(j)(k).toDouble / repeat.toDouble
      }
    }
  }

  for (i <- avg.indices) {
    val t = T(i)
    println(s"T = $t")
    println(avg(i).map(_.mkString(" ")).mkString("\n"))
  }

}
