package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.benchmark.charts.PerformanceProfileChart
import village1.search.cp.LNSOptions
import village1.util.{FileUtils, Utils}

// --T=15,15,15,15,15 --D=50,50,50 --W=300,300,300
object CPLimitBenchmark extends CommandLineBenchmark with Branchings {

  val name = s"cp-limit-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json").copy(probabilities = Map("assignWorkingRequirements" -> 0)))

  val base = LNSOptions()
  val limits = Array(2500, 5000, 7500, 10000, 15000, 20000)
  val lnsOptions = limits.map(l => base.copy(limit = l))
  val names = limits.map(l => s"L=$l")

  val benchmark = new BenchmarkRunner(options = options)

  val results = lnsOptions.zip(names).map { arg =>
    val (_, o, _) = benchmark.run(arg._2, solveCP(benchmark, lnsOptions = arg._1))
    o
  }


  val values = results.map(_.means)

  val profile = PerformanceProfile.generate(values, values, names)
  FileUtils.writeFile(s"data/benchmark/profile/$name.json", profile)
  PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name.html")
}
