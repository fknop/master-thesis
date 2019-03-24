package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}
import village1.json.JsonSerializer
import village1.util.Utils


object CPHeuristicBenchmark extends CommandLineBenchmark with Branchings {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-heuristic${Utils.randomInt(0, 1000)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP-MA", solveCP(benchmark))
  val (t1, o1) = benchmark.run("CP-FF", solveCP(benchmark, applyToSearch = search => {
    search.heuristic {
      binaryFirstFail(search.flatWorkers)
    }
  }))

  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
