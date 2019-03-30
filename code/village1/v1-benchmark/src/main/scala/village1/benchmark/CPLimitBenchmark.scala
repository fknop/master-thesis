package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.benchmark.charts.PerformanceProfileChart
import village1.search.cp.LNSOptions
import village1.util.{FileUtils, Utils}


object CPLimitBenchmark extends CommandLineBenchmark with Branchings {

  val name = s"cp-limit-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val limits = Array(1000, 1500, 2000, 2500, 3000)
  val lnsOptions = limits.map(l => LNSOptions().copy(limit = l))
  val names = limits.map(l => s"L=$l")

  val benchmark = new SolverBenchmark(options = options)

  val results = lnsOptions.zip(names).map { arg =>
    val (_, o) = benchmark.run(arg._2, solveCP(benchmark, lnsOptions = arg._1))
    o
  }


  val values = results.map(_.means)

  val profile = PerformanceProfile.generate(values, values, names)
  FileUtils.writeFile(s"data/benchmark/profile/$name.json", profile)
  PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name.html")
}
