package village1.benchmark.api

import village1.benchmark.util.MathUtils._
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.{Problem, VillageOneModel}
import village1.search.SolutionEmitter

import scala.util.Random


class BenchmarkRunner(
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
    val instanceOptions = InstanceOptions(
        t = T(t),
        clients = D(d), // This parameter doesn't really matter
        demands = D(d),
        workers = W(w),
        skills = 10,
        machines = 0,
        locations = 0
      )

    generator.generate(
      instanceOptions.copy(probabilities = instanceOptions.probabilities ++ options.probabilities)
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

  private def normalizeSerie(serie: BenchmarkOverTimeSerie, t: Int, bests: Array[Int]): BenchmarkOverTimeNSerie = {
    val results = serie.results
    val normalized: Array[Array[(Int, Double)]] = Array.fill(t * 10, serie.results.length)(null)


    for (j <- results.indices) {
      val best = bests(j)
      val result = results(j)
      val first = result.last._2
      var values = result

      var time = values.last._1

      var value = 1.0
      for (i <- 0 until (t * 10)) {
        while (i * 100 >= time) {
          if (values.last._2 == best) {
            value = 0.0
          }
          else {
            value = (values.last._2 - best).toDouble / (first - best).toDouble
          }

          values = values.init
          if (values.nonEmpty) {
            time = values.last._1
          }
          else {
            time = Long.MaxValue
          }
        }

        normalized(i)(j) = (i, value)
      }
    }

    val measurements = Array.fill[BenchmarkMeasurement](t * 10)(null)
    for (i <- 0 until (t * 10)) {
      val measures = normalized(i).map(_._2)
      measurements(i) = BenchmarkMeasurement(
        measures.min,
        measures.max,
        mean(measures),
        stdDev(measures)
      )
    }

    for (i <- 1 until measurements.length) {
      assert(measurements(i).mean <= measurements(i - 1).mean)
    }

    BenchmarkOverTimeNSerie(serie.name, measurements)
  }

  def normalize(t: Int, series: BenchmarkOverTimeSerie*): Array[BenchmarkOverTimeNSerie] = {
    val results: Array[Array[List[(Long, Int)]]] = series.map(_.results).toArray
    val bests = Array.fill[Int](results(0).length)(0)

    for (i <- bests.indices) {
      bests(i) = results.map(_(i).head._2).min
    }

    series.map(s => normalizeSerie(s, t, bests)).toArray
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



  def run (name: String, solver: VillageOneModel => (SolutionEmitter, () => (Long, Int))): (BenchmarkSerie, BenchmarkSerie, BenchmarkOverTimeSerie) = {
    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(Repeat)(0L))
    val objectiveOverTime = Array.fill(T.length * D.length * W.length * Repeat)(List[(Long, Int)]())

    log("Start Dry Runs")
    for (_ <- 0 until DryRun) {
      val model = baseModels(0)(0)(0)
      solver(model)._2()
    }
    log("End Dry Run")
    log("Start Runs")

    var l = 0
    for (r <- 0 until Repeat) {
      log(s"Start Run $r")

      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)

            val (emitter, solve) = solver(baseModel)

            emitter.onSolutionFound {
              solution =>
                objectiveOverTime(l) ::= (solution.time, solution.objective)
            }

            val result = solve()

            if (result == null) {
              timeMeasurements(i)(j)(k)(r) = -1
              objectiveMeasurements(i)(j)(k)(r) = -1
            }
            else {
              val (time, objective) = result
              timeMeasurements(i)(j)(k)(r) = time
              objectiveMeasurements(i)(j)(k)(r) = objective
            }

            l += 1
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

    (BenchmarkSerie(name, timeSerie), BenchmarkSerie(name, objectiveSerie), BenchmarkOverTimeSerie(name, objectiveOverTime))
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


