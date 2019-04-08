package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.charts.PerformanceProfileChart
import village1.benchmark.util.MathUtils
import village1.search.cp.heuristic.MostAvailableHeuristic
import village1.util.{FileUtils, Utils}


object CPHeuristicBenchmark2 extends CommandLineBenchmark with Branchings {

  val name = s"cp-heuristic-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))
  println(MathUtils.estimatedTime(options, 2))


  val benchmark = new BenchmarkRunner(options = options)

  val names = Array("CP-MA-D", "CP-MA")
  val (t0, o0) = benchmark.run(names(0), solveCP(benchmark))
  val (t1, o1) = benchmark.run(names(1), solveCP(benchmark, applyToSearch = search => {
    search.heuristic = new MostAvailableHeuristic(search, search.flatWorkers, search.workerVariables)
  }))

  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))

  val values = Array(o0.means, o1.means)

  val profile = PerformanceProfile.generate(values, values, names)
  val profile2 = PerformanceProfile.generate(Array(values(0)), values, names)
  val profile3 = PerformanceProfile.generate(Array(values(1)), values, names)

  FileUtils.writeFile(s"data/benchmark/profile/$name.json", profile)
  PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name.html")
  PerformanceProfileChart.generate(profile2)(s"data/benchmark/html/$name-${names(0)}.html")
  PerformanceProfileChart.generate(profile3)(s"data/benchmark/html/$name-${names(1)}.html")
  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}
