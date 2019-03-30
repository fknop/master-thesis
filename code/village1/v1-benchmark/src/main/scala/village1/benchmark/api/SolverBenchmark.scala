package village1.benchmark.api

import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.{Problem, VillageOneModel}
import village1.benchmark.util.MathUtils._

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

  val sizes: Array[ProblemSize] = Array.tabulate(T.length, D.length, W.length)( (t, d, w) => ProblemSize(T(t), D(d), W(w))).flatten.flatten

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

  val baseModels: Array[Array[Array[VillageOneModel]]] = Array.tabulate(T.length, D.length, W.length) { (t, d, w) =>
      val problem = generate(t, d, w)
      val model = new VillageOneModel(problem)
      model
  }


  def makeInstance(timeSeries: Seq[BenchmarkSerie], objectiveSeries: Seq[BenchmarkSerie]): BenchmarkInstance = {
    BenchmarkInstance(
      Repeat,
      DryRun,
      TimeLimit,
      SolutionLimit,
      sizes,
      timeSeries,
      objectiveSeries
    )
  }

  def lowerBoundSerie(): BenchmarkSerie = {
    val results = Array.fill[BenchmarkMeasurement](T.length * D.length * W.length)(null)
    var i = 0
    for (t <- T.indices; d <- D.indices; w <- W.indices) {
      val model = baseModels(t)(d)(w)
      val problem = model.problem
      var lb = 0
      for (demand <- problem.demands) {
        lb += demand.requiredWorkers
      }

      results(i) = BenchmarkMeasurement(lb, lb, lb, 0)
      i += 1
    }


    BenchmarkSerie("Lowerbound", results)
  }


  def run (name: String, solve: VillageOneModel => (Long, Int)): (BenchmarkSerie, BenchmarkSerie) = {
    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))

    log("Start Dry Runs")
    for (_ <- 0 until DryRun) {
      val model = baseModels(0)(0)(0)
      solve(model)
    }
    log("End Dry Run")

    log("Start Runs")

    for (r <- 0 until Repeat) {
      log(s"Start Run $r")

      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)
            val result = solve(baseModel)
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

      log(s"End run: $r")
    }

    log("End Runs")
    log("Start measurements")

    val timeSerie = Array.fill[BenchmarkMeasurement](T.length * D.length * W.length)(null)
    val objectiveSerie = Array.fill[BenchmarkMeasurement](T.length * D.length * W.length)(null)
    var r = 0
    for (i <- T.indices) {
      for (j <- D.indices) {
        for (k <- W.indices) {
          val timeMeasurement = getMeasurements(timeMeasurements(i)(j)(k))
          val objectiveMeasurement = getMeasurements(objectiveMeasurements(i)(j)(k))
          timeSerie(r) = timeMeasurement
          objectiveSerie(r) = objectiveMeasurement
          r += 1
        }
      }
    }

    log("End measurements")

    (BenchmarkSerie(name, timeSerie), BenchmarkSerie(name, objectiveSerie))
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


