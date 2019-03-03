package village1.benchmark

import village1.generator.InstanceGenerator
import village1.modeling.VillageOneModel
import village1.util.Benchmark._


class SolverBenchmark(
   val T: Array[Int] = Array(5),
   val D: Array[Int] = Array(30, 50),
   val W: Array[Int] = Array(100, 200, 300),
   val SolutionLimit: Int = 1,
   val TimeLimit: Int = 30,
   val Repeat: Int = 5,
   val DryRun: Int = 1
 ) {

  val baseModels: Array[Array[Array[VillageOneModel]]] = Array.tabulate(T.length, D.length, W.length) { (t, d, w) =>
    val problem = InstanceGenerator.generate(
      t = T(t),
      c = D(d), // This parameter doesn't really matter
      d = D(d),
      w = W(w),
      s = 10,
      prob = Map("skill" -> 0.2, "period" -> 0.6)
    )

    new VillageOneModel(problem)
  }


  def run (solve: VillageOneModel => (Long, Int)): Array[BenchmarkInstance] = {

    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))

    for (r <- -DryRun until Repeat) {
      val measure = r >= 0
      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)
            val (time, objective) = solve(baseModel)
            if (measure) {
              timeMeasurements(i)(j)(k)(r) = time
              objectiveMeasurements(i)(j)(k)(r) = objective
            }
          }
        }
      }
    }

    val results = Array.fill[BenchmarkInstance](T.length)(null)
    for (i <- T.indices) {
      val values = Array.fill[BenchmarkResult](D.length * W.length)(null)
      var v = 0
      for (j <- D.indices) {
        for (k <- W.indices) {
          val problemSize = ProblemSize(D(j), W(k))
          val timeMeasurement = getMeasurements(timeMeasurements(i)(j)(k))
          val objectiveMeasurement = getMeasurements(objectiveMeasurements(i)(j)(k))
          values(v) = BenchmarkResult(problemSize, timeMeasurement, objectiveMeasurement)
          v += 1
        }
      }

      results(i) = BenchmarkInstance(T(i), values)
    }

    results
  }

  private def getMeasurements(measurements: Array[Long]): BenchmarkMeasurement = {
    BenchmarkMeasurement(
      measurements.min,
      measurements.max,
      mean(measurements),
      stdDev(measurements)
    )
  }

//  private def solveCPThenMIP (base: VillageOneModel): (Long, Int) = {
//    val cp = new VillageOneSearch(base)
//    val stat = cp.solve(nSols = 1, timeLimit = TimeLimit * 1000, silent = true)
//
//    val remaining = TimeLimit - (stat.time / 1000).toInt
//
//
//    val model = new VillageOneMIPModel(base)
//    model.initialize(withObjective = true)
//
//    if (cp.lastSolution != null) {
//      model.setInitialSolution(cp.lastSolution)
//    }
//
//    val solver: SolverResult = model.solve(silent = true, timeLimit = remaining, nSols = SolutionLimit)
//    solver.dispose()
//    (solver.solveTime + stat.time, solver.solution.objective)
//  }
//
//  private def solveMIP (base: VillageOneModel): (Long, Int) = {
//    val model = new VillageOneMIPModel(base)
//    model.initialize(withObjective = true)
//    val solver: SolverResult = model.solve(silent = true, timeLimit = TimeLimit, nSols = SolutionLimit)
//    solver.dispose()
//
//    (solver.solveTime, solver.solution.objective)
//  }
//
//  private def solveCP (base: VillageOneModel): (Long, Int) = {
//    val search = new VillageOneLNS(base)
//    val stats = search.solve(nSols = SolutionLimit, timeLimit = TimeLimit * 1000, silent = true)
//    assert(search.lastSolution != null)
//    (stats, search.lastSolution.objective)
//  }
//
//  private def solveCPDefaultHeuristic (base: VillageOneModel): (Long, Int) = {
//    val search = new VillageOneLNS(base)
//    val stats = search.solve(nSols = SolutionLimit, timeLimit = TimeLimit * 1000, silent = true, heuristic = SearchHeuristic.Default)
//    assert(search.lastSolution != null)
//    (stats, search.lastSolution.objective)
//  }
}


