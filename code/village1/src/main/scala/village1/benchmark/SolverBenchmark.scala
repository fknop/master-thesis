package village1.benchmark

import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.{Problem, VillageOneModel}
import village1.util.BenchmarkUtils._

import scala.util.Random



class SolverBenchmark(
   val options: BenchmarkOptions
 ) {


  val Repeat: Int = options.repeat
  val DryRun: Int = options.dryRun
  val SolutionLimit: Int = options.solutionLimit
  val TimeLimit: Int = options.timeLimit
  val T: Array[Int] = options.T
  val D: Array[Int] = options.D
  val W: Array[Int] = options.W

  val seed: Long = if (options.seed == -1L) Random.nextLong() else options.seed
  private val generator = new InstanceGenerator(seed)

  log(options)
  log(s"Seed=$seed")

  private def log(message: Any): Unit = {
    println(message)
  }

  private def generate(t: Int, d: Int, w: Int): Problem = {
    generator.generate(
      InstanceOptions(
        t = T(t),
        clients = D(d), // This parameter doesn't really matter
        demands = D(d),
        workers = W(w),
        skills = 10,
        machines = 20,
        locations = 20
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


  def makeInstance(series: BenchmarkSerie*): BenchmarkInstance = {
    BenchmarkInstance(
      Repeat,
      DryRun,
      TimeLimit,
      SolutionLimit,
      series
    )
  }


  def run (name: String, solve: VillageOneModel => (Long, Int)): BenchmarkSerie = {
    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))

    log("Start run")

    for (r <- -DryRun until Repeat) {
      val measure = r >= 0
      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)(r + DryRun)
            val result = solve(baseModel)
            if (measure) {
              if (result == null) {
                timeMeasurements(i)(j)(k)(r) = -1
                objectiveMeasurements(i)(j)(k)(r) = -1
              }
              else {
                val (time, objective) = result
                timeMeasurements(i)(j)(k)(r) = time
                objectiveMeasurements(i)(j)(k)(r) = objective
              }
            }
          }
        }
      }

      log(s"End run: $r")
    }

    log("End run")
    log("Start measurements")

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

    log("End measurements")

    BenchmarkSerie(name, results)
  }

  private def getMeasurements(measurements: Array[Long]): BenchmarkMeasurement = {

    val filtered = measurements.filter(_ >= 0)

    if (filtered.isEmpty) {
      BenchmarkMeasurement(0, 0, 0, 0)
    }
    else {
      BenchmarkMeasurement(
        filtered.min,
        filtered.max,
        mean(filtered),
        stdDev(filtered)
      )
    }
  }
}


