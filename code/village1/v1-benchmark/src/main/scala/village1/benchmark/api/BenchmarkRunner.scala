package village1.benchmark.api

import play.api.libs.json.Json
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.charts.{LineChart, OOTChart, PerformanceProfileChart}
import village1.benchmark.util.MathUtils
import village1.benchmark.util.MathUtils._
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.{Problem, VillageOneModel}
import village1.search.SolutionEmitter
import village1.util.FileUtils

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
  private val rand = new Random(seed)

  log(options)
  log(s"Seed=$seed")

  private def log(message: Any): Unit = {
    println(message)
  }

  private def generate(t: Int, d: Int, w: Int): Problem = {

    val probabilities = Map(
      "assignSkill" -> randDouble(rand, 0.1, 0.3),
      "assignWorkerSkill" -> randDouble(rand, 0.1, 0.3),
      "assignPeriod" -> randDouble(rand, 0.4, 0.8),
      "assignLocation" -> randDouble(rand, 0.3, 0.7),
      "assignMachines" -> randDouble(rand, 0.1, 0.5),
      "takeMachine" -> randDouble(rand, 0.1, 0.3),
      "assignWorkingRequirements" -> randDouble(rand, 0.1, 0.3),
      "assignWWI" -> randDouble(rand, 0, 0.1),
      "assignWCI" -> randDouble(rand, 0, 0.1)
    )

    val instanceOptions = InstanceOptions(
        t = T(t),
        clients = D(d), // This parameter doesn't really matter
        demands = D(d),
        workers = W(w),
        skills = 10,
        machines = 30,
        locations = 30
      )

    generator.generate(
      instanceOptions.copy(probabilities = probabilities ++ options.probabilities)
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

  private def normalizeSerie(serie: BenchmarkOverTimeSerie, t: Int, bests: Array[Int], worsts: Array[Int]): BenchmarkOverTimeNSerie = {
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

    BenchmarkOverTimeNSerie(serie.name, measurements)
  }

  def normalize(series: BenchmarkOverTimeSerie*): Array[BenchmarkOverTimeNSerie] = {
    val results: Array[Array[List[(Long, Int)]]] = series.map(_.results).toArray
    val bests = Array.fill[Int](results(0).length)(0)
    val worsts = Array.fill[Int](results(0).length)(0)

    for (i <- bests.indices) {
      bests(i) = results.map(_(i).head._2).min
      worsts(i) = results.map(_(i).last._2).max
    }

    series.map(s => normalizeSerie(s, options.timeLimit, bests, worsts)).toArray
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

object BenchmarkRunner {


  def run(
     name: String,
     options: BenchmarkOptions,
     names: Array[String],
     solvers: Array[BenchmarkRunner => VillageOneModel => (SolutionEmitter, () => (Long, Int))]): Unit = {

    assert(names.length == solvers.length)
    println("Benchmark: " + name)
    println(MathUtils.estimatedTime(options, names.length))

    val runner = new BenchmarkRunner(options)
    val path = "data/benchmark/"

    val series: Seq[(BenchmarkSerie, BenchmarkSerie, BenchmarkOverTimeSerie)] = names.zip(solvers.map(_(runner))).map(v => runner.run(v._1, v._2))
    val ootSeries = series.map(_._3)

    import village1.benchmark.api.json.JsonBenchmark.benchmarkOOTSerieWrites
    val ootJson = Json.prettyPrint(
      Json.toJson(ootSeries)
    )

    FileUtils.writeFile(s"$path/$name/oot.json", ootJson)

    val normalized = runner.normalize(ootSeries: _*)
    OOTChart.generate(normalized)(s"$path/$name/oot.html")

    val lb = runner.lowerBoundSerie()
    val instance = runner.makeInstance(timeSeries = series.map(_._1), objectiveSeries = series.map(_._2) ++ Seq(lb))
    val writer = JsonBenchmark.serialize(instance)
    writer(s"$path/$name/instance.json")

    import JsonBenchmark._
    LineChart.generate(Json.prettyPrint(Json.toJson(instance)))(s"$path/$name/line-charts.html")


    val values = series.map(_._2.means)

    val baselines: Seq[Seq[Int]] = values.indices.foldLeft(Seq[Seq[Int]]()) {
      (acc, x) => acc ++ values.indices.combinations(x + 1)
    }

    for (baseline <- baselines) {
      val profile = PerformanceProfile.generate(baseline.map(values(_)).toArray, values.toArray, names)
      val bName = baseline.map(names(_)).mkString(",")
      PerformanceProfileChart.generate(profile)(s"$path/$name/profile-B=$bName.html")
    }
  }
}

object Test extends App {
//  import JsonBenchmark._
//  val path = "data/benchmark/cp,mip,cp+mip685/oot2.json"
//  val json = JsonUtils.parseJsonFile(path)
//  val result = Json.fromJson[Seq[BenchmarkOverTimeSerie]](json)
//  var series: Seq[BenchmarkOverTimeSerie] = null
//  result match {
//    case JsSuccess(b: Seq[BenchmarkOverTimeSerie], _) =>  series = b
//    case e: JsError => throw JsResultException(e.errors)
//  }
//
//
//  val runner = new BenchmarkRunner(options = BenchmarkArgs(timeLimit = 30))
//  val normalized = runner.normalize(series: _*)
//
//  println(series.map(_.results(0).map(_._2)).mkString("\n"))
//  println(normalized.map(_.means.mkString(" ")).mkString("\n"))
//
//  OOTChart.generate(normalized)("data/benchmark/test.html")


  val instance = JsonBenchmark.parse("data/remote/benchmark/cp-relaxations-589/instance.json")

  val series = instance.objectiveSeries.map(serie => (serie.name, serie.means)).filterNot(_._1 == "Lowerbound")
  val sizes = instance.problems
  var bySize: Map[ProblemSize, List[Double]] = Map()


  var results = Map[String, Int]()
  var sizesBySolver = Map[String, List[ProblemSize]]()

  for (j <- sizes.indices) {
    val best = series.minBy(_._2(j))
    val name = best._1
    results = results.updated(name, results.getOrElse(name, 0) + 1)

    sizesBySolver = sizesBySolver.updated(name, sizes(j) :: sizesBySolver.getOrElse(name, List[ProblemSize]()))

  }


  for (key <- sizesBySolver.keys) {
    val values = sizesBySolver(key).groupBy(identity).mapValues(_.size)
    println(key)
    val sizes = values.keys.toArray.sorted
    for (size <- sizes) {
      println(size, values(size))
    }
  }
}
