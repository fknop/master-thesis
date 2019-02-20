package village1.benchmark

import village1.benchmark.PrecomputeBenchmark.{D, T, W, measurements}
import village1.generator.InstanceGenerator
import village1.modeling.{Problem, VillageOneModel}
import village1.modeling.mip.{SolverResult, VillageOneMIPModel2}
import village1.search.cp.VillageOneSearch

object SolverBenchmark extends App {

  run()

  def run (timeLimit: Int = 60): Unit = {
    val T = Array(5, 10, 15)
    val D = Array(5, 10, 20, 30, 40, 50)
    val W = Array(50, 100, 150, 200, 250, 300)

    val measurements = Array.fill(T.length, D.length, W.length)((0L, 0L))

    for (i <- T.indices) {
      val t = T(i)
      for (j <- D.indices) {
        val d = D(j)
        for (k <- W.indices) {
          val w = W(k)
          val problem = InstanceGenerator.generate(
            t = t,
            c = d, // This parameter doesn't really matter
            d = d,
            w = w,
            s = 10,
            prob = Map("skill" -> 0.2, "period" -> 0.6)
          )

          val cpTime = solveCP(problem, timeLimit)
          val mipTime = solveMIP(problem, timeLimit)

          // Make sure the JVM has JIT compiled the code before measuring
          measurements(i)(j)(k) = (cpTime, mipTime)
          println(measurements(i)(j)(k))
        }
      }
    }
  }

  private def solveMIP (problem: Problem, timeLimit: Int): Long = {
    val model = new VillageOneMIPModel2(problem)
    model.initialize()
    val solver: SolverResult = model.solve()
    solver.dispose()

    solver.solveTime
  }

  private def solveCP (problem: Problem, timeLimit: Int): Long = {
    val search = new VillageOneSearch(problem)
    val stats = search.solve(1, timeLimit)
    stats.time
  }
}
