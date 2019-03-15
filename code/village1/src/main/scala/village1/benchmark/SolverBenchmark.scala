package village1.benchmark

import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.{Problem, VillageOneModel}
import village1.util.BenchmarkUtils._



class SolverBenchmark(
   val T: Array[Int] = Array(5),
   val D: Array[Int] = Array(30, 50),
   val W: Array[Int] = Array(100, 200, 300),
   val options: BenchmarkOptions
 ) {

  val Repeat: Int = options.repeat
  val DryRun: Int = options.dryRun
  val SolutionLimit: Int = options.solutionLimit
  val TimeLimit: Int = options.timeLimit

  private def generate(t: Int, d: Int, w: Int): Problem = {
    InstanceGenerator.generate(
      InstanceOptions(
        t = T(t),
        clients = D(d), // This parameter doesn't really matter
        demands = D(d),
        workers = W(w),
        skills = 10,
        machines = 20,
        locations = 20,
        probabilities = Map("skill" -> 0.2, "period" -> 0.6)
      )
    )
  }

  val baseModels: Array[Array[Array[Array[VillageOneModel]]]] = Array.tabulate(T.length, D.length, W.length) { (t, d, w) =>

    if (!options.noKeep) {
      val problem = generate(t, d, w)
      val model = new VillageOneModel(problem)
      Array.fill[VillageOneModel](Repeat + DryRun)(model)
    }
    else {
      Array.tabulate(Repeat + DryRun)(_ => {
        val problem = generate(t, d, w)
        new VillageOneModel(problem)
      })
    }

  }


  def makeInstance(series: (String, Array[BenchmarkResult])*): BenchmarkInstance = {
    BenchmarkInstance(
      Repeat,
      DryRun,
      TimeLimit,
      SolutionLimit,
      series.map(s => BenchmarkSerie(s._1, s._2))
    )
  }


  def run (solve: VillageOneModel => (Long, Int)): Array[BenchmarkResult] = {
    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))

    for (r <- -DryRun until Repeat) {
      val measure = r >= 0
      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)(r + DryRun)
            val (time, objective) = solve(baseModel)
            if (measure) {
              timeMeasurements(i)(j)(k)(r) = time
              objectiveMeasurements(i)(j)(k)(r) = objective
            }
          }
        }
      }
    }

    val results = Array.fill[BenchmarkResult](T.length * D.length * W.length)(null)
    var r = 0
    for (i <- T.indices) {
      for (j <- D.indices) {
        for (k <- W.indices) {
          val problemSize = ProblemSize(T(i), D(j), W(k))
          val timeMeasurement = getMeasurements(timeMeasurements(i)(j)(k))
          val objectiveMeasurement = getMeasurements(objectiveMeasurements(i)(j)(k))
          results(r) = BenchmarkResult(problemSize, timeMeasurement, objectiveMeasurement)
          r += 1
        }
      }
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
}


