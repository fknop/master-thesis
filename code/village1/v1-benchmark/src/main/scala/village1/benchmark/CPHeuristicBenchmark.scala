package village1.benchmark

import oscar.algo.search.Branching
import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}
import village1.search.cp.heuristic.Heuristic
import village1.util.Utils


object CPHeuristicBenchmark extends CommandLineBenchmark with Branchings {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-heuristic${Utils.randomInt(0, 1000)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP-MA", solveCP(benchmark))
  val (t1, o1) = benchmark.run("CP-FF", solveCP(benchmark, applyToSearch = search => {
    search.heuristic {
      new Heuristic {
        override def branching: Branching = binaryFirstFail(search.flatWorkers)
      }
    }
  }))

  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}
