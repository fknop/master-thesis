package village1.benchmark

import oscar.algo.search.Branching
import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.charts.PerformanceProfileChart
import village1.benchmark.util.MathUtils
import village1.search.cp.heuristic.MostAvailableHeuristicDynamic
import village1.util.Utils


object CPHeuristicBenchmark3 extends CommandLineBenchmark with Branchings {

  val name = s"cp-heuristic-cos${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))
  println(MathUtils.estimatedTime(options, 3))


  val benchmark = new BenchmarkRunner(options = options)

  val names = Array("MD", "COS", "MS")
  val (t0, o0, oot0) = benchmark.run(names(0), solveCP(benchmark))

  val (t1, o1, oot1) = benchmark.run(names(1), solveCP(benchmark, applyToSearch = search => {
    class H extends MostAvailableHeuristicDynamic(search, search.flatWorkers, search.workerVariables) {
      override def branching: Branching = conflictOrderingSearch(x, varHeuristic, valueHeuristic)
    }

    search.heuristic = new H()
  }))

  val (t2, o2, oot2) = benchmark.run(names(1), solveCP(benchmark, applyToSearch = search => {
    class H extends MostAvailableHeuristicDynamic(search, search.flatWorkers, search.workerVariables) {
      override def branching: Branching = binaryIdx(x, i => x(i).max, valueHeuristic)
    }

    search.heuristic = new H()
  }))



  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))

  val values = Array(o0.means, o1.means, o2.means)

  val baselines = Array(
    Array(0, 1, 2),
    Array(0),
    Array(1),
    Array(2),
    Array(0, 1),
    Array(0, 2),
    Array(1, 2)
  )

  for (baseline <- baselines) {
    val profile = PerformanceProfile.generate(baseline.map(values(_)), values, names)
    val bName = baseline.map(names(_)).mkString(",")
    PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-B=$bName.html")
  }

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}
